;; -*- lexical-binding: t; -*-

(use-package ox-jira
  :after org)

(use-package org-jira
  :defer t)

(setq org-jira-entry-mode-map nil)

(defvar cat-org-jira-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") 'org-jira-get-projects)
    (define-key map (kbd "b") 'org-jira-get-boards)
    (define-key map (kbd "ii") 'org-jira-get-issues)
    (define-key map (kbd "ib") 'org-jira-get-issues-by-board)
    (define-key map (kbd "ij") 'org-jira-get-issues-from-custom-jql)
    (define-key map (kbd "iv") 'org-jira-get-issues-by-fixversion)
    (define-key map (kbd "ih") 'org-jira-get-issues-headonly)
    (define-key map (kbd "if") 'org-jira-get-issues-from-filter-headonly)
    (define-key map (kbd "iF") 'org-jira-get-issues-from-filter)
    (define-key map (kbd "io") 'org-jira-browse-issue)
    (define-key map (kbd "iu") 'org-jira-update-issue)
    (define-key map (kbd "iw") 'org-jira-progress-issue)
    (define-key map (kbd "in") 'org-jira-progress-issue-next)
    (define-key map (kbd "ia") 'org-jira-assign-issue)
    (define-key map (kbd "is") 'org-jira-set-issue-reporter)
    (define-key map (kbd "ir") 'org-jira-refresh-issue)
    (define-key map (kbd "iR") 'org-jira-refresh-issues-in-buffer)
    (define-key map (kbd "ic") 'org-jira-create-issue)
    (define-key map (kbd "ik") 'org-jira-copy-current-issue-key)
    (define-key map (kbd "sc") 'org-jira-create-subtask)
    (define-key map (kbd "sg") 'org-jira-get-subtasks)
    (define-key map (kbd "cc") 'org-jira-add-comment)
    (define-key map (kbd "cu") 'org-jira-update-comment)
    (define-key map (kbd "wu") 'org-jira-update-worklogs-from-org-clocks)
    (define-key map (kbd "tj") 'org-jira-todo-to-jira)
    map))
(defalias 'cat-org-jira-prefix cat-org-jira-map)
