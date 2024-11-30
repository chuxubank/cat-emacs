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
    (format "Activity %s" (activities-name-for activity))))

(cl-defmethod treemacs-scope->setup ((_ (subclass treemacs-activities-scope)))
  "Activities-scope setup."
  (advice-add 'activities-define :after #'treemacs-activities--on-activity-switch)
  (advice-add 'activities-resume :after #'treemacs-activities--on-activity-switch)
  (add-hook 'activities-after-switch-functions #'treemacs-activities--on-activity-switch)
  (treemacs-activities--ensure-workspace-exists))

(cl-defmethod treemacs-scope->cleanup ((_ (subclass treemacs-activities-scope)))
  "Activities-scope tear-down."
  (advice-remove 'activities-define #'treemacs-activities--on-activity-switch)
  (advice-remove 'activities-resume #'treemacs-activities--on-activity-switch)
  (remove-hook 'activities-after-switch-functions #'treemacs-activities--on-activity-switch))

(defun treemacs-activities--on-activity-switch (activity &rest _)
  "Hook running after the activity was switched or resumed.
Will select a workspace for the now active activity ACTIVITY, creating it if necessary."
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

(defun treemacs-activities--create-workspace (name)
  "Create a new workspace for the activity with NAME.
Projects will be found as per `treemacs--find-user-project-functions'.  If that
does not return anything the projects of the fallback workspace will be copied."
  (treemacs-block
   (let* ((ws-result (treemacs-do-create-workspace name))
          (ws-status (car ws-result))
          (ws (cadr ws-result))
          (root-path (treemacs--find-current-user-project))
          (project-list))
     (unless (eq ws-status 'success)
       (treemacs-log "Failed to create workspace for activity: %s, using fallback instead." ws)
       (treemacs-return (car treemacs--workspaces)))
     (if root-path
         (setf project-list
               (list (treemacs-project->create!
                      :name (treemacs--filename root-path)
                      :path root-path
                      :path-status (treemacs--get-path-status root-path))))
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

(provide 'treemacs-activities)
;;; treemacs-activities.el ends here
