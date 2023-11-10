;; -*- lexical-binding: t; -*-

(use-package ox-jira
  :after org)

(use-package org-jira
  :commands #'org-jira-id
  :bind
  (:map org-jira-entry-mode-map
        ("C-c n j" . cat-org-jira-issue-prefix))
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
  (org-jira-default-jql "assignee = currentUser() AND resolution = Unresolved AND statusCategory in (2, 4) order by updated DESC")
  (org-jira-custom-jqls
   '((:jql "assignee in (EMPTY) AND project = Android AND issuetype = Bug AND status = Open AND fixVersion in unreleasedVersions() AND Scrubbed = Scrubbed ORDER BY created DESC"
           :limit 10
           :filename "bug-backlog")
     (:jql "assignee = currentUser() AND Sprint in openSprints() AND resolution = Unresolved order by created DESC"
           :limit 50
           :filename "cur-sprint")))
  (org-jira-progress-issue-flow
   '(("In Progress" . "PR is created")
     ("Code Review" . "Ready for testing")))
  :config
  (add-hook 'org-jira-mode-hook #'cat-hide-trailing-whitespace)
  (add-to-list 'org-agenda-files (expand-file-name "cur-sprint.org" org-jira-working-dir)))

(defun +org-jira-copy-current-issue-url ()
  (interactive)
  (kill-new (concat (replace-regexp-in-string "/*$" "" jiralib-url) "/browse/" (org-jira-id))))

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
