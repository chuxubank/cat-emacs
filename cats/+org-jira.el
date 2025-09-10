;; -*- lexical-binding: t; -*-

(use-package ox-jira
  :demand t
  :after org)

(use-package task
  :ensure nil)

(use-package org-jira
  :delight " 󰌃"
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
  :mode-hydra
  (org-mode
   ("Jira"
    (("j" cat-org-jira-dispatch "org jira dispatch"))))
  :config
  (+mkdir-p org-jira-working-dir)
  (add-hook 'org-jira-mode-hook #'cat-hide-trailing-whitespace)
  (add-to-list 'org-agenda-files org-jira-working-dir))

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

(defun +org-jira-start-dev-work (issue-key action-id &rest _args)
  "Create a branch with ISSUE-KEY and current org heading content if ACTION-ID is to start work."
  (let* ((open-next (cdr (assoc "Open" jiralib-available-actions-cache)))
         (start-action '("Start Dev Work" "Work Started"))
         (start-action-id (mapcar (lambda (pair)
                                    (when (member (cdr pair) start-action)
                                      (car pair)))
                                  open-next))
         (org-heading (nth 4 (org-heading-components))))
    (when (member action-id start-action-id)
      (task-create-branch-with-key-and-text (magit-read-repository) "develop" issue-key org-heading))))

(advice-add 'jiralib-progress-workflow-action :after #'+org-jira-start-dev-work)

(defun cat-org-jira-dispatch ()
  "If `org-jira-mode' is active, show Hydra; else push current TODO to JIRA."
  (interactive)
  (if (bound-and-true-p org-jira-mode)
      (cat-org-jira-issue/body)
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
  "s" #'+org-jira-save-jql-files
  "d" #'+org-jira-delete-custom-jql-files)

(pretty-hydra-define cat-org-jira-issue
  (:title "Org-Jira Issue" :color teal :quit-key "q" :foreign-keys warn)
  ("Navigation"
   (("b" org-jira-browse-issue "Browse issue")
    ("w" org-jira-copy-current-issue-key "Copy issue key")
    ("W" +org-jira-copy-current-issue-url "Copy URL"))
   
   "Comments"
   (("c" org-jira-update-comment "Update comment")
    ("C" org-jira-add-comment "Add comment"))
   
   "Issue Management"
   (("a" org-jira-assign-issue "Assign issue")
    ("u" org-jira-update-issue "Update issue")
    ("r" org-jira-set-issue-reporter "Set reporter"))
   
   "Progress"
   (("p" org-jira-progress-issue "Progress")
    ("n" org-jira-progress-issue-next "Next progress"))
   
   "Subtasks"
   (("t" org-jira-get-subtasks "Get subtasks")
    ("T" org-jira-create-subtask "Create subtask"))

   "Refresh"
   (("g" org-jira-refresh-issue "Refresh issue")
    ("G" org-jira-refresh-issues-in-buffer "Refresh all issues"))

   "Worklogs"
   (("l" org-jira-update-worklogs-from-org-clocks "Update from org clocks"))
   ))
