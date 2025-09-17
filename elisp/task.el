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

(defcustom task-jira-default-jql "assignee = currentUser() and resolution = Unresolved"
  "The default JQL to filter Jira tasks."
  :type 'string
  :group 'task)

(defcustom task-jira-search-limit 20
  "The Jira search limit."
  :type 'number
  :group 'task)

(unless (file-exists-p task-icon-cache-dir)
  (make-directory task-icon-cache-dir t))

(defvar task--jira-candidates nil
  "Cache for last JIRA candidates.")

(defun task--generate-branch-name (key text)
  "Make KEY and TEXT a valid branch name."
  (concat key "-" (downcase (replace-regexp-in-string "[^A-Za-z]+" "-" text))))

(defun task--get-icon (url key)
  "Get image icon from URL with KEY for cache."
  (let* ((cache-file (expand-file-name key task-icon-cache-dir)))
    (unless (file-exists-p cache-file)
      (url-copy-file url cache-file t))
    (create-image cache-file nil nil :ascent 'center)))

(defun task--format-time (time-str)
  "Parse and format the TIME-STR with format string."
  (format-time-string "%Y-%m-%d %H:%M" (parse-iso8601-time-string time-str)))

(defun task--jira-format-candidates (issues)
  "Format completion candidates for Jira ISSUES."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (issue issues)
      (let* ((key         (cdr (assoc 'key issue)))
             (fields      (cdr (assoc 'fields issue)))
             (created     (cdr (assoc 'created fields)))
             (updated     (cdr (assoc 'updated fields)))
             (summary     (cdr (assoc 'summary fields)))
             (created-fmt (task--format-time created))
             (updated-fmt (task--format-time updated))
             (display (format
                       "%-12s [%s | %s]  %s"
                       key
                       created-fmt
                       updated-fmt
                       summary)))
        (puthash display issue table)))
    table))

(defun task--jira-get-icon (type)
  "Get icon for TYPE in Jira."
  (when-let* ((typename (car type))
              (fields   (cdr type))
              (name     (cdr (assoc 'name fields)))
              (iconUrl  (cdr (assoc 'iconUrl fields)))
              (id       (cdr (assoc 'id fields)))
              (img (task--get-icon iconUrl (format "jira-%s-%s" typename id))))
    (propertize name 'display img)))

(defun task--jira-completion-table (candidates)
  "Return a completion table for Jira CANDIDATES."
  (let* ((metadata `(metadata
                     (category . jira-issue)
                     (affixation-function
                      . ,(lambda (completions)
                           (mapcar (lambda (cand)
                                     (let* ((issue     (gethash cand candidates))
                                            (fields    (cdr (assoc 'fields issue)))
                                            (issuetype (assoc 'issuetype fields))
                                            (priority  (assoc 'priority fields)))
                                       (list cand
                                             (concat
                                              (task--jira-get-icon priority)
                                              " "
                                              (task--jira-get-icon issuetype)
                                              " ")
                                             "")))
                                   completions)))
                     (group-function
                      . ,(lambda (cand transform)
                           (let* ((issue     (gethash cand candidates))
                                  (fields    (cdr (assoc 'fields issue)))
                                  (issuetype (cdr (assoc 'issuetype fields)))
                                  (typename  (cdr (assoc 'name issuetype)))
                                  (desc      (cdr (assoc 'description issuetype))))
                             (if transform
                                 cand
                               (format "%s - %s" typename desc))))))))
    (lambda (string pred action)
      (if (eq action 'metadata)
          metadata
        (complete-with-action action candidates string pred)))))

(defun task-jira-select-issue (jql)
  "Prompt user to select a Jira issue using JQL.
Both key and summary participate in completion matching, but the
return value is always the issue key."
  (interactive "sJQL: ")
  (let* ((cands (task--jira-format-candidates
                 (jiralib-do-jql-search jql task-jira-search-limit)))
         (table (task--jira-completion-table cands))
         (cand (completing-read "Select JIRA issue: " table)))
    (gethash cand cands)))

(defun task-create-branch-with-key-and-text (key text)
  "Use KEY and TEXT as name to create branch."
  (let* ((branch (magit-read-other-branch
                  "Branch name: "
                  nil
                  (task--generate-branch-name key text))))
    (with-suppressed-warnings ((interactive-only magit-branch-or-checkout))
      (magit-branch-or-checkout branch))))

(defun task-pull-remote-branch (&optional remote branch)
  "Pull BRANCH from REMOTE."
  (interactive
   (list (magit-read-remote "Select remote: ")
         (magit-read-starting-point "Branch")))
  (if (string= branch (magit-get-current-branch))
      (magit-pull-branch branch nil)
    (magit-fetch-refspec remote (format "%s:%s" branch branch) nil)))

;;;###autoload
(defun task-start-dev-work (&optional pull-first)
  "Start the development with task.

If PULL-FIRST, will run task to pull the remote branch first."
  (interactive)
  (let* ((issue   (task-jira-select-issue task-jira-default-jql))
         (key     (cdr (assoc 'key issue)))
         (fields  (cdr (assoc 'fields issue)))
         (summary (cdr (assoc 'summary fields))))
    (when pull-first
      (task-pull-remote-branch))
    (task-create-branch-with-key-and-text key summary)))

(provide 'task)
;;; task.el ends here
