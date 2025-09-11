;; task.el --- A task manage tool works with Magit like JetBrains plugin -*- lexical-binding: t; -*-

;; Author: Misaka <chuxubank@qq.com>
;; Maintainer: Misaka <chuxubank@qq.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1") (magit "3.0.0") (jiralib "0.3"))
;; Keywords: tools, git, jira
;; URL: https://github.com/chuxubank/task.el

;;; Commentary:

;; This package provides an integration between Emacs, Magit, and JIRA to
;; streamline starting development work from JIRA issues.
;;
;; Features:
;;
;; - Query JIRA issues using JQL (default or custom).
;; - Display candidate issues in `completing-read` with affixation,
;;   showing metadata such as created/updated dates, summary, and
;;   issue type icon.
;; - Cache JIRA issue type icons locally for reuse.
;; - Start development work by creating a Git branch named after the
;;   selected JIRA issue key and summary.
;; - Supports selecting the remote and source branch interactively
;;   if not already on the desired branch.
;;
;; Typical workflow:
;;
;;   1. Run `M-x task-start-dev-work`.
;;   2. Select an issue from JIRA with completion (issues are grouped by type).
;;   3. A new branch is created and checked out, based on the issue key
;;      and summary.
;;
;; This helps developers quickly pick up tasks, follow a consistent
;; branch naming scheme, and reduce context-switching between tools.
;;
;; Requirements:
;; - `magit' for Git operations.
;; - `jiralib' for JIRA integration.
;;
;; Configuration:
;; - `task-jira-default-jql' defines the default query for fetching issues.
;; - `task-icon-cache-dir' defines where issue type icons are cached.
;;
;; Example:
;;
;;   (setq task-jira-default-jql "assignee = currentUser() AND status = 'In Progress'")
;;   (require 'task)
;;
;; Then run:
;;
;;   M-x task-start-dev-work
;;
;; This will let you select an issue and automatically prepare a branch
;; to start coding.

;;; Code:

(require 'magit)
(require 'jiralib)

(defcustom task-icon-cache-dir "~/.cache/emacs/task/icon"
  "Cache directory for task icon."
  :type 'directory
  :group 'task)

(defcustom task-jira-default-jql "assignee = currentUser() and resolution = Unresolved order by updated"
  "The default jql to filter task."
  :type 'string
  :group 'task)

(unless (file-exists-p task-icon-cache-dir)
  (make-directory task-icon-cache-dir t))

(defvar task--jira-candidates nil
  "Cache for last JIRA candidates.")

(defun task--generate-branch-name (text)
  "Make TEXT a valid branch name."
  (replace-regexp-in-string "[^A-Za-z]+" "-" text))

(defun task--jira-issues-candidates (jql limit)
  "Return an alist of (ISSUE-KEY . ISSUE-DATA) for JQL with LIMIT."
  (let ((issues (jiralib-do-jql-search jql limit)))
    (mapcar (lambda (issue)
              (cons (cdr (assoc 'key issue)) issue))
            issues)))

(defun task--get-icon (url key)
  "Get image icon from URL with KEY for cache."
  (let* ((cache-file (expand-file-name key task-icon-cache-dir)))
    (unless (file-exists-p cache-file)
      (url-copy-file url cache-file t))
    (create-image cache-file nil nil :ascent 'center)))

(defun task-jira-select-issue (jql)
  "Prompt user to select a JIRA issue using JQL, with affixation (date/summary)."
  (interactive "sJQL: ")
  (let* ((cands (task--jira-issues-candidates jql 50))
         (affix-fn
          (lambda (completions)
            (mapcar (lambda (cand)
                      (when-let* ((issue (cdr (assoc cand cands)))
                                  (fields (cdr (assoc 'fields issue)))
                                  (created (cdr (assoc 'created fields)))
                                  (updated (cdr (assoc 'updated fields)))
                                  (summary (cdr (assoc 'summary fields)))
                                  (issuetype (cdr (assoc 'issuetype fields)))
                                  (iconUrl (cdr (assoc 'iconUrl issuetype)))
                                  (typeId (cdr (assoc 'id issuetype))))
                        (list (propertize cand 'display (format "%-10s" cand))
                              (concat
                               (when-let ((img (task--get-icon iconUrl (concat "jira-" typeId))))
                                 (propertize " " 'display img))
                               " ")
                              (format " %-10s  %-10s  %s"
                                      (or created "")
                                      (or updated "")
                                      (or summary "")))))
                    completions)))
         (group-fn
          (lambda (cand trans)
            (let* ((issue (cdr (assoc cand cands)))
                   (fields (cdr (assoc 'fields issue)))
                   (issuetype (cdr (assoc 'issuetype fields)))
                   (typename (cdr (assoc 'name issuetype)))
                   (desc (cdr (assoc 'description issuetype))))
              (if trans
                  cand
                (format "%s - %s" typename desc)))))
         (completion-extra-properties
          `(:affixation-function ,affix-fn :group-function ,group-fn)))
    (setq task--jira-candidates cands)
    (completing-read "Select JIRA issue: " task--jira-candidates nil t)))

(defun task-create-branch-with-key-and-text (key text &optional remote source-branch)
  "Use KEY and TEXT as name to create branch from REMOTE's SOURCE-BRANCH in REPO."
  (let* ((remote (or remote
                     (magit-read-remote "Select remote")))
         (branch (or source-branch
                     (magit-read-branch "Select source branch"))))
    (if (string= branch (magit-get-current-branch))
        (magit-pull-branch branch nil)
      (magit-fetch-refspec remote (format "%s:%s" branch branch) nil))
    (with-suppressed-warnings ((interactive-only magit-branch-and-checkout))
      (magit-branch-and-checkout
       (read-string
        "Branch name: "
        (concat key "-" (downcase (task--generate-branch-name text))))
       branch))))

;;;###autoload
(defun task-start-dev-work ()
  "Start the development with task."
  (interactive)
  (let* ((key (task-jira-select-issue task-jira-default-jql))
         (issue (cdr (assoc key task--jira-candidates)))
         (fields (cdr (assoc 'fields issue)))
         (summary (cdr (assoc 'summary fields))))
    (task-create-branch-with-key-and-text key summary)))

(provide 'task)
;;; task.el ends here
