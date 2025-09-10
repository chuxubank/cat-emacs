;; -*- lexical-binding: t; -*-

(require 'magit)
(require 'jiralib)

(defvar task--jira-candidates nil
  "Cache for last JIRA candidates.")

(defun task--generate-branch-name (text)
  "Make TEXT a valid branch name."
  (replace-regexp-in-string "[^A-Za-z]+" "-" text))

(defun task-create-branch-with-key-and-text (repo source-branch key text)
  "Use KEY and TEXT as name to create branch from SOURCE-BRANCH in REPO."
  (magit-status repo)
  (if (string= source-branch (magit-get-current-branch))
      (magit-pull-branch source-branch nil)
    (magit-fetch-refspec "origin" (format "%s:%s" source-branch source-branch) nil))
  (magit-branch-and-checkout
   (read-string "Branch name: "
                (concat key "-" (downcase (task--generate-branch-name text))))
   source-branch))

(defun task--jira-issues-candidates (jql limit)
  "Return an alist of (ISSUE-KEY . ISSUE-DATA) for JQL with LIMIT."
  (let ((issues (jiralib-do-jql-search jql limit)))
    (mapcar (lambda (issue)
              (cons (cdr (assoc 'key issue)) issue))
            issues)))

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
                                  (typename (cdr (assoc 'name issuetype))))
                        (list (format "%-10s" cand)
                              (format "%-10s" (or typename ""))
                              (format " %-10s  %-10s  %s"
                                      (or created "")
                                      (or updated "")
                                      (or summary "")))))
                    completions)))
         (completion-extra-properties
          `(:affixation-function ,affix-fn)))
    (completing-read "Select JIRA issue: " cands nil t)))

(provide 'task)

