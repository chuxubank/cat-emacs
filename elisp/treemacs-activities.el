;;; treemacs-activities.el --- Activities integration for treemacs -*- lexical-binding: t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: xuchu1
;; Package-Requires: ((emacs "29.1") (treemacs "0.0") (activities "0.8") (dash "2.11.0"))
;; Homepage: https://github.com/alphapapa/activities.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;; Integration of activities into treemacs' buffer scoping framework.

;;; Code:

(require 'treemacs)
(require 'activities)
(require 'eieio)
(require 'dash)
(require 'project)
(require 'map)

(eval-when-compile
  (require 'treemacs-macros)
  (require 'cl-lib))

(defclass treemacs-activities-scope (treemacs-scope) () :abstract t)
(add-to-list 'treemacs-scope-types (cons 'Activities 'treemacs-activities-scope))

(cl-defmethod treemacs-scope->current-scope ((_ (subclass treemacs-activities-scope)))
  "Get the current activity as scope.
Returns the symbol `none' if no activity is active."
  (or (activities-current) 'none))

(cl-defmethod treemacs-scope->current-scope-name ((_ (subclass treemacs-activities-scope)) activity)
  "Return the name of the given ACTIVITY.
Will return \"No Activity\" if no activity is active."
  (if (eq 'none activity)
      "No Activity"
    (treemacs-activities--format-workspace-name (activities-activity-name activity))))

(defun treemacs-activities--on-activity-kill (activity)
  "Hook running before an activity is killed.
Will delete the treemacs workspace for ACTIVITY."
  (treemacs--on-scope-kill activity))

(cl-defmethod treemacs-scope->setup ((_ (subclass treemacs-activities-scope)))
  "Activities-scope setup."
  ;; (advice-add 'activities-set :after #'treemacs-activities--on-activity-switch)
  (advice-add 'activities-rename :before #'treemacs-activities--on-activity-rename)
  (advice-add 'activities-discard :before #'treemacs-activities--on-activity-kill)
  (add-hook 'bookmark-after-jump-hook #'treemacs-activities--on-activity-switch)
  (add-hook 'activities-after-switch-functions #'treemacs-activities--on-activity-switch)
  (treemacs-activities--ensure-workspace-exists))

(cl-defmethod treemacs-scope->cleanup ((_ (subclass treemacs-activities-scope)))
  "Activities-scope tear-down."
  ;; (advice-remove 'activities-set #'treemacs-activities--on-activity-switch)
  (advice-remove 'activities-rename #'treemacs-activities--on-activity-rename)
  (advice-remove 'activities-discard #'treemacs-activities--on-activity-kill)
  (remove-hook 'bookmark-after-jump-hook #'treemacs-activities--on-activity-switch)
  (remove-hook 'activities-after-switch-functions #'treemacs-activities--on-activity-switch))

(defun treemacs-activities--on-activity-rename (activity name)
  "Hook running before activity is renamed.
Will rename treemacs activity workspace from ACTIVITY's current name to NAME.
Return t on success, nil otherwise.
Should be run before `activities-rename' to ensure workspace name stays in sync."
  (treemacs-block
   (let* ((old-name (treemacs-scope->current-scope-name
                     (treemacs-current-scope-type) activity))
          (new-name (treemacs-activities--format-workspace-name name))
          (workspace (treemacs--find-workspace-by-name old-name)))
     (unless workspace
       (treemacs-log-err "Could not find workspace for activity %s" old-name)
       (treemacs-return nil))
     (-let [(success msg) (treemacs-do-rename-workspace workspace new-name)]
       (if success
           (progn
             (treemacs-log "Renamed workspace from '%s' to '%s'" old-name new-name)
             (treemacs-return t))
         (user-error "Failed to rename treemacs workspace: %s" msg))))))

(defun treemacs-activities--on-activity-switch (&rest _)
  "Hook running after the activity was switched or resumed.
Will select a workspace for the now active activity, creating it if necessary."
  (treemacs-without-following
   (treemacs-activities--ensure-workspace-exists)
   (treemacs--change-buffer-on-scope-change)))

(defun treemacs-activities--ensure-workspace-exists ()
  "Make sure a workspace exists for the current activity.
Matching happens by name. If no workspace can be found it will be created."
  (let* ((activity-name (treemacs-scope->current-scope-name
                         (treemacs-current-scope-type) (treemacs-current-scope)))
         (workspace (or (treemacs--find-workspace-by-name activity-name)
                        (treemacs-activities--create-workspace activity-name))))
    (setf (treemacs-current-workspace) workspace)
    (treemacs--invalidate-buffer-project-cache)
    (run-hooks 'treemacs-switch-workspace-hook)
    workspace))

(defun treemacs-activities--activity-buffers ()
  "Return the list of buffers belonging to the current activity.
When `activities-tabs-mode' is active, use the tab's buffer list.
Otherwise walk the activity's saved window state to collect filenames
and find corresponding live buffers."
  (if (bound-and-true-p activities-tabs-mode)
      (-when-let* ((activity (activities-current))
                   (tab (activities-tabs--tab activity)))
        (activities-tabs--tab-parameter 'activities-buffer-list tab))
    ;; Fallback: extract filenames from the saved state and find live buffers.
    (-when-let* ((activity (activities-current))
                 (state (or (activities-activity-last activity)
                            (activities-activity-default activity)))
                 (window-state (activities-activity-state-window-state state)))
      (let (buffers)
        (treemacs-activities--walk-window-state
         window-state
         (lambda (ab)
           (-when-let* ((filename (activities-buffer-filename ab))
                        (buf (find-buffer-visiting filename)))
             (cl-pushnew buf buffers))))
        buffers))))

(defun treemacs-activities--walk-window-state (state fn)
  "Walk window STATE tree, calling FN with each `activities-buffer' found."
  (pcase state
    (`(leaf . ,attrs)
     (-when-let (ab (map-elt (map-elt attrs 'parameters) 'activities-buffer))
       (funcall fn ab)))
    ((pred listp)
     (dolist (child state)
       (treemacs-activities--walk-window-state child fn)))))

(defun treemacs-activities--find-project-roots ()
  "Collect unique project roots from the current activity's buffers.
Returns a list of canonical project root paths."
  (let (roots seen)
    (dolist (buf (or (treemacs-activities--activity-buffers)
                     ;; Ultimate fallback: all file-visiting buffers.
                     (-filter #'buffer-file-name (buffer-list))))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (-when-let (project (project-current))
            (let ((root (-> project (project-root) (file-truename) (treemacs-canonical-path))))
              (unless (member root seen)
                (push root seen)
                (push root roots)))))))
    (nreverse roots)))

(defun treemacs-activities--create-workspace (name)
  "Create a new workspace for the activity with NAME.
Projects will be found by scanning all file-visiting buffers for project roots.
Falls back to `treemacs--find-current-user-project' if no buffer-based roots
are found.  If that also fails, the projects of the fallback workspace will be
copied."
  (treemacs-block
   (let* ((ws-result (treemacs-do-create-workspace name))
          (ws-status (car ws-result))
          (ws (cadr ws-result))
          (root-paths (or (treemacs-activities--find-project-roots)
                          (-when-let (single (treemacs--find-current-user-project))
                            (list single))))
          (project-list))
     (unless (eq ws-status 'success)
       (treemacs-log "Failed to create workspace for activity: %s, using fallback instead." ws)
       (treemacs-return (car treemacs--workspaces)))
     (if root-paths
         (dolist (root-path root-paths)
           (push (treemacs-project->create!
                  :name (treemacs--filename root-path)
                  :path root-path
                  :path-status (treemacs--get-path-status root-path))
                 project-list))
       (-let [fallback-workspace (car treemacs--workspaces)]
         ;; copy the projects instead of reusing them so we don't accidentally rename
         ;; a project in 2 workspaces
         (dolist (project (treemacs-workspace->projects fallback-workspace))
           (push (treemacs-project->create!
                  :name (treemacs-project->name project)
                  :path (treemacs-project->path project)
                  :path-status (treemacs-project->path-status project))
                 project-list))))
     (setf (treemacs-workspace->projects ws) (nreverse project-list))
     (treemacs-return ws))))

(defun treemacs-activities--format-workspace-name (activity-name)
  (format "Activity %s" activity-name))

(provide 'treemacs-activities)
;;; treemacs-activities.el ends here
