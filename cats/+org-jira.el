;; -*- lexical-binding: t; -*-

(use-package ox-jira
  :demand t
  :after org)

(use-package org-jira
  :delight " 󰌃"
  :init
  ;; prevent `org-jira-mode' load keymap
  (setq org-jira-entry-mode-map (make-sparse-keymap))
  :custom
  (org-jira-done-states
   '("Closed" "Resolved" "Done" "Cancelled"))
  (org-jira-jira-status-to-org-keyword-alist
   '(("In Progress" . "STRT")
     ("Code Review" . "WAIT")
     ("QA Ready" . "LOOP")))
  (org-jira-default-jql "project != RNA AND assignee = currentUser() AND statusCategory in ('To Do', 'In Progress') order by updated DESC")
  (org-jira-custom-jqls
   '((:jql "project != RNA AND assignee in (currentUser()) AND statusCategory = 'To Do' AND (Sprint is EMPTY OR Sprint not in openSprints()) ORDER BY priority DESC, updated DESC"
           :limit 10
           :filename "todo")
     (:jql "assignee = currentUser() AND statusCategory in ('To Do', 'In Progress') AND Sprint in openSprints() ORDER BY priority DESC, updated DESC"
           :limit 50
           :filename "cur-sprint")))
  (org-jira-progress-issue-flow
   '(("Open" . "Start Dev Work")
     ("In Progress" . "PR is created")
     ("Code Review" . "Ready for testing")))
  :config
  (add-hook 'org-jira-mode-hook #'cat-hide-trailing-whitespace)
  (add-to-list 'org-agenda-files (expand-file-name "cur-sprint.org" org-jira-working-dir)))

(defun +org-jira-copy-current-issue-url ()
  "Copy current jira issue url."
  (interactive)
  (kill-new (concat (replace-regexp-in-string "/*$" "" jiralib-url) "/browse/" (org-jira-id))))

(defun +org-jira-delete-custom-jql-files ()
  "Delete cached custom jql files."
  (interactive)
  (dolist (jql org-jira-custom-jqls)
    (let ((filename (cl-getf jql :filename)))
      (when filename
        (delete-file (expand-file-name (concat filename ".org") org-jira-working-dir))))))

(defun +org-jira-save-jql-files ()
  "Save cached jql files."
  (interactive)
  (save-some-buffers t (lambda ()
                         (and (derived-mode-p 'org-mode)
                              (string-prefix-p
                               (expand-file-name org-jira-working-dir)
                               (file-name-directory (buffer-file-name)))))))

(defun +org-jira-get-issues-from-custom-jql ()
  "Custom `org-jira-get-issues-from-custom-jql'."
  (interactive)
  (require 'org-jira)
  (+org-jira-delete-custom-jql-files)
  (org-jira-get-issues-from-custom-jql))

(defun cat-generate-branch-name (text)
  "Make TEXT a valid branch name."
  (replace-regexp-in-string "[^A-Za-z]+" "-" text))

(defun cat-create-branch-with-key-and-text (source-branch key text)
  "Use KEY and TEXT as name to create branch from SOURCE-BRANCH."
  (let ((root (magit-read-repository)))
    (magit-status root)
    (if (string= source-branch (magit-get-current-branch))
        (magit-pull-branch "develop" nil)
      (magit-fetch-refspec "origin" (format "%s:%s" source-branch source-branch) nil))
    (magit-branch-and-checkout
     (read-string "Branch name: "
                  (concat key (downcase (cat-generate-branch-name text))))
     source-branch)))

(defun cat-org-jira-start-dev-work (issue-key action-id params &optional callback)
  "Helper function to create a branch with ISSUE-KEY and current org heading content."
  (let* ((open-next (cdr (assoc "Open" jiralib-available-actions-cache)))
         (start-action '("Start Dev Work" "Work Started"))
         (start-action-id (mapcar (lambda (pair)
                                    (when (member (cdr pair) start-action)
                                      (car pair)))
                                  open-next))
         (org-heading (nth 4 (org-heading-components))))
    (when (member action-id start-action-id)
      (cat-create-branch-with-key-and-text "develop" issue-key org-heading))))
(advice-add 'jiralib-progress-workflow-action :after #'cat-org-jira-start-dev-work)

(defvar-keymap org-jira-global-map
  :doc "Keymap for `org-jira' global commands."
  :name "Org Jira"
  :prefix 'cat-org-jira-prefix
  "b" #'org-jira-get-boards
  "c" #'org-jira-create-issue
  "h" #'org-jira-get-issues-headonly
  "i" #'org-jira-get-issue
  "I" #'org-jira-get-issues
  "j" #'+org-jira-get-issues-from-custom-jql
  "p" #'org-jira-get-projects
  "v" #'org-jira-get-issues-by-fixversion)

(defvar-keymap org-jira-issue-map
  :doc "Keymap for `org-jira' issue commands."
  :name "Org Jira Issue"
  :prefix 'cat-org-jira-issue-prefix
  "a" #'org-jira-assign-issue
  "b" #'org-jira-browse-issue
  "c" #'org-jira-update-comment
  "C" #'org-jira-add-comment
  "g" #'org-jira-refresh-issue
  "G" #'org-jira-refresh-issues-in-buffer
  "j" #'org-jira-todo-to-jira
  "l" #'org-jira-update-worklogs-from-org-clocks
  "n" #'org-jira-progress-issue-next
  "p" #'org-jira-progress-issue
  "r" #'org-jira-set-issue-reporter
  "t" #'org-jira-get-subtasks
  "T" #'org-jira-create-subtask
  "u" #'org-jira-update-issue
  "w" #'org-jira-copy-current-issue-key
  "W" #'+org-jira-copy-current-issue-url)
