;; -*- lexical-binding: t; -*-

(use-package ox-jira
  :after org)

(use-package org-jira
  :defer t
  :init
  ;; prevent `org-jira-mode' load keymap
  (setq org-jira-entry-mode-map nil)
  (defvar org-jira-map (make-sparse-keymap)
    "Keymap for `org-jira' commands.")
  (defalias 'org-jira-prefix org-jira-map)
  :bind
  (:map org-jira-map
        ("b" . org-jira-get-boards)
        ("p" . org-jira-get-projects)
        ("t" . org-jira-todo-to-jira)
        :prefix "i"
        :prefix-map org-jira-issue-map
        :prefix-docstring "Keymap for `org-jira' issue commands."
        ("a" . org-jira-assign-issue)
        ("b" . org-jira-browse-issue)
        ("c" . org-jira-create-issue)
        ("g" . org-jira-get-issue)
        ("k" . org-jira-copy-current-issue-key)
        ("n" . org-jira-progress-issue-next)
        ("p" . org-jira-progress-issue)
        ("r" . org-jira-refresh-issue)
        ("R" . org-jira-refresh-issues-in-buffer)
        ("s" . org-jira-set-issue-reporter)
        ("u" . org-jira-update-issue)
        ("w" . org-jira-update-worklogs-from-org-clocks)
        :prefix "s"
        :prefix-map org-jira-issue-subtask-map
        :prefix-docstring "Keymap for `org-jira' issue subtask commands."
        ("c" . org-jira-create-subtask)
        ("g" . org-jira-get-subtasks)
        :prefix "c"
        :prefix-map org-jira-issue-comment-map
        :prefix-docstring "Keymap for `org-jira' issue comment commands."
        ("c" . org-jira-add-comment)
        ("u" . org-jira-update-comment))
  (:map org-jira-issue-map
        :prefix "g"
        :prefix-map org-jira-issues-get-map
        :prefix-docstring "Keymap for `org-jira' issue get commands."
        ("b" . org-jira-get-issues-by-board)
        ;; ("f" . org-jira-get-issues-from-filter-headonly)
        ;; ("F" . org-jira-get-issues-from-filter)
        ("g" . org-jira-get-issues)
        ("h" . org-jira-get-issues-headonly)
        ("j" . org-jira-get-issues-from-custom-jql)
        ("v" . org-jira-get-issues-by-fixversion)))
