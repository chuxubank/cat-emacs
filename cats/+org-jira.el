;; -*- lexical-binding: t; -*-

(use-package ox-jira
  :after org)

(use-package org-jira
  :bind
  (:map org-jira-entry-mode-map
        ("C-c n j" . cat-org-jira-issue-prefix))
  :init
  ;; prevent `org-jira-mode' load keymap
  (setq org-jira-entry-mode-map (make-sparse-keymap)))

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
  "k" #'org-jira-copy-current-issue-key
  "n" #'org-jira-progress-issue-next
  "p" #'org-jira-progress-issue
  "r" #'org-jira-set-issue-reporter
  "t" #'org-jira-get-subtasks
  "T" #'org-jira-create-subtask
  "u" #'org-jira-update-issue
  "w" #'org-jira-update-worklogs-from-org-clocks)
