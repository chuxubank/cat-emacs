;; -*- lexical-binding: t; -*-

(use-package ox-jira
  :demand t
  :after org)

(use-package task
  :vc (:url "https://github.com/cat-emacs/task.el")
  :commands
  (task-create-branch-from-fields
   task-pull-remote-branch))

(use-package org-jira
  :delight (org-jira-mode (:eval (+with-icon "nf-md-jira" " ")))
  :after org
  :init
  ;; prevent `org-jira-mode' load keymap
  (setq org-jira-entry-mode-map (make-sparse-keymap))
  :custom
  (jiralib-url (getenv "JIRA_URL"))
  (org-jira-download-comments nil)
  (org-jira-done-states
   '("Closed" "Resolved" "Done" "Cancelled"))
  (org-jira-jira-status-to-org-keyword-alist
   '(("In Progress" . "STRT")
     ("Code Review" . "WAIT")
     ("QA Ready" . "LOOP")))
  (org-jira-default-jql "assignee = currentUser() AND resolution = Unresolved order by updated DESC")
  (org-jira-custom-jqls
   '((:jql "project != RNA AND assignee in (currentUser()) AND statusCategory = 'To Do' AND (Sprint is EMPTY OR Sprint not in openSprints()) ORDER BY priority DESC, updated DESC"
           :limit 10
           :filename "todo")
     (:jql "assignee = currentUser() AND statusCategory in ('To Do', 'In Progress') AND Sprint in openSprints() ORDER BY priority DESC, updated DESC"
           :limit 50
           :filename "cur-sprint")))
  (org-jira-progress-issue-flow
   '(("Open" . "Work Started")
     ("In Progress" . "PR is created")
     ("Code Review" . "Ready for testing")))
  :major-transient
  (org-mode
   ["Plugin"
    ("j" "org jira dispatch" cat/org-jira-dispatch)])
  :config
  (+mkdir-p org-jira-working-dir)
  (add-hook 'org-jira-mode-hook #'cat/hide-trailing-whitespace)
  (add-to-list 'org-agenda-files org-jira-working-dir))

(defun cat/org-jira-copy-current-issue-url ()
  "Copy current jira issue url."
  (interactive)
  (kill-new (concat (replace-regexp-in-string "/*$" "" jiralib-url) "/browse/" (org-jira-id))))

(defun cat/org-jira-delete-custom-jql-files ()
  "Delete cached custom jql files."
  (interactive)
  (dolist (jql org-jira-custom-jqls)
    (let ((filename (cl-getf jql :filename)))
      (when filename
        (delete-file (expand-file-name (concat filename ".org") org-jira-working-dir))))))

(defun cat/org-jira-save-jql-files ()
  "Save cached jql files."
  (interactive)
  (save-some-buffers t (lambda ()
                         (and (derived-mode-p 'org-mode)
                              (string-prefix-p
                               (expand-file-name org-jira-working-dir)
                               (file-name-directory (buffer-file-name)))))))

(defun cat/org-jira-start-dev-work (issue-key action-id &rest _args)
  "Create a branch with ISSUE-KEY and current org heading content if ACTION-ID is to start work."
  (let* ((open-next (cdr (assoc "Open" jiralib-available-actions-cache)))
         (start-action '("Start Dev Work" "Work Started"))
         (start-action-id (mapcar (lambda (pair)
                                    (when (member (cdr pair) start-action)
                                      (car pair)))
                                  open-next))
         (org-heading (nth 4 (org-heading-components))))
    (when (member action-id start-action-id)
      (magit-read-repository)
      (task-pull-remote-branch)
      (task-create-branch-from-fields issue-key org-heading))))

(advice-add 'jiralib-progress-workflow-action :after #'cat/org-jira-start-dev-work)

(defun cat/org-jira-dispatch ()
  "Show issue commands when `org-jira-mode' is active.
Otherwise push the current TODO to JIRA."
  (interactive)
  (if (bound-and-true-p org-jira-mode)
      (cat-org-jira-issue)
    (call-interactively #'org-jira-todo-to-jira)))

(defvar-keymap org-jira-global-map
  :doc "Keymap for `org-jira' global commands."
  :name "Org Jira"
  :prefix 'cat-org-jira-prefix
  "b" #'org-jira-get-boards
  "c" #'org-jira-create-issue
  "h" #'org-jira-get-issues-headonly
  "i" #'org-jira-get-issue
  "I" #'org-jira-get-issues
  "j" #'org-jira-get-issues-from-custom-jql
  "p" #'org-jira-get-projects
  "v" #'org-jira-get-issues-by-fixversion
  "s" #'cat/org-jira-save-jql-files
  "d" #'cat/org-jira-delete-custom-jql-files)

(mode-transient-define-prefix cat-org-jira-issue ()
  :description "Org-Jira Issue"
  ["Navigation"
   ("b" "Browse issue" org-jira-browse-issue)
   ("w" "Copy issue key" org-jira-copy-current-issue-key)
   ("W" "Copy URL" cat/org-jira-copy-current-issue-url)]
  ["Comments"
   ("c" "Update comment" org-jira-update-comment)
   ("C" "Add comment" org-jira-add-comment)]
  ["Issue Management"
   ("a" "Assign issue" org-jira-assign-issue)
   ("u" "Update issue" org-jira-update-issue)
   ("r" "Set reporter" org-jira-set-issue-reporter)]
  ["Progress"
   ("p" "Progress" org-jira-progress-issue)
   ("n" "Next progress" org-jira-progress-issue-next)]
  ["Subtasks"
   ("t" "Get subtasks" org-jira-get-subtasks)
   ("T" "Create subtask" org-jira-create-subtask)]
  ["Refresh"
   ("g" "Refresh issue" org-jira-refresh-issue)
   ("G" "Refresh all issues" org-jira-refresh-issues-in-buffer)]
  ["Worklogs"
   ("l" "Update from org clocks" org-jira-update-worklogs-from-org-clocks)])
