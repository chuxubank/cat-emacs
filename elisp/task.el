;; -*- lexical-binding: t; -*-

(require 'magit)
(require 'jiralib)

(defcustom task-icon-cache-dir "~/.cache/emacs/task/icon"
  "Cache dir for task icon")

(unless (file-exists-p task-icon-cache-dir)
  (make-directory task-icon-cache-dir t))

(defvar task--jira-candidates nil
  "Cache for last JIRA candidates.")

(defun task--generate-branch-name (text)
  "Make TEXT a valid branch name."
  (replace-regexp-in-string "[^A-Za-z]+" "-" text))

(defun task-create-branch-with-key-and-text (repo key text &optional remote source-branch)
  "Use KEY and TEXT as name to create branch from REMOTE's SOURCE-BRANCH in REPO."
  (magit-status-setup-buffer repo)
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
                      (when-let* ((pair (assoc cand cands))
                                  (issue (cdr pair))
                                  (fields (cdr (assoc 'fields issue)))
                                  (created (cdr (assoc 'created fields)))
                                  (updated (cdr (assoc 'updated fields)))
                                  (summary (cdr (assoc 'summary fields)))
                                  (issuetype (cdr (assoc 'issuetype fields)))
                                  (iconUrl (cdr (assoc 'iconUrl issuetype)))
                                  (typeId (cdr (assoc 'id issuetype)))
                                  (typename (cdr (assoc 'name issuetype))))
                        (list (format "%-10s" cand)
                              (concat
                               (when-let ((img (task--get-icon iconUrl (concat "jira-" typeId))))
                                 (propertize " " 'display img))
                               " ")
                              (format " %-10s  %-10s  %s"
                                      (or created "")
                                      (or updated "")
                                      (or summary "")))))
                    completions)))
         (completion-extra-properties
          `(:affixation-function ,affix-fn)))
    (completing-read "Select JIRA issue: " cands nil t)))

(provide 'task)

