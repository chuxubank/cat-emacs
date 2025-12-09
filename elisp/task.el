;; task.el --- A task manage tool works with Magit like JetBrains plugin -*- lexical-binding: t; -*-

;; Author: Misaka <chuxubank@qq.com>
;; Maintainer: Misaka <chuxubank@qq.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1") (magit "3.0.0") (jira "2.0"))
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
;; - `jira' for JIRA integration.
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
(require 'jira-api)
(require 'jira-issues)
(require 'json)

(defcustom task-icon-cache-dir "~/.cache/emacs/task/icon"
  "Cache directory for task icon."
  :type 'directory
  :group 'task)

(defcustom task-backend 'acli-jira
  "Backend used to fetch issues.

Supported backends:
- `jira' (default): use the Emacs jira package.
- `acli-jira': use the `acli jira` command-line tool."
  :type '(choice (const :tag "JIRA API" jira)
                 (const :tag "acli jira" acli-jira))
  :group 'task)

(defcustom task-jira-default-jql "assignee = currentUser() and resolution = Unresolved"
  "The default JQL to filter Jira tasks."
  :type 'string
  :group 'task)

(defcustom task-jira-search-limit 20
  "The Jira search limit."
  :type 'number
  :group 'task)

(defcustom task-acli-jira-command '("acli" "jira")
  "Base command (program + subcommand) for the `acli jira` backend.

Customize this if you need to inject additional global flags, e.g. auth or
profile arguments. The JQL, limit, and JSON flags are appended automatically."
  :type '(repeat string)
  :group 'task)

(defcustom task-acli-jira-extra-args '("--output" "json")
  "Extra arguments passed to the `acli jira search` command.

The JQL and limit arguments are appended automatically."
  :type '(repeat string)
  :group 'task)

(unless (file-exists-p task-icon-cache-dir)
  (make-directory task-icon-cache-dir t))

(defvar task--jira-candidates nil
  "Cache for last JIRA candidates.")

(defun task--jira-api-search-sync (jql max-results)
  "Synchronously search JIRA issues using JQL.
Returns the issues list from the API response."
  (let* ((response (jira-api-search :params `(("jql" . ,jql)
                                              ("maxResults" . ,max-results)
                                              ("fields" . "key,summary,created,updated,issuetype,priority,status"))
                                    :sync t))
         (data (request-response-data response)))
    (cdr (assoc 'issues data))))

(defun task--acli-jira--get (issue key)
  "Get KEY from ISSUE, looking into `fields' when present."
  (or (alist-get key issue)
      (alist-get key (alist-get 'fields issue))))

(defun task--acli-jira--build-type (issue symbol name-key icon-key id-key)
  "Build a TYPE entry (SYMBOL . alist) from ISSUE using NAME-KEY, ICON-KEY, ID-KEY."
  (or (alist-get symbol (alist-get 'fields issue))
      (let ((name (task--acli-jira--get issue name-key))
            (icon (task--acli-jira--get issue icon-key))
            (id   (task--acli-jira--get issue id-key)))
        (when (or name icon id)
          (cons symbol (delq nil (list (and name (cons 'name name))
                                       (and icon (cons 'iconUrl icon))
                                       (and id   (cons 'id id)))))))))

(defun task--acli-jira--build-status (issue)
  "Normalize status payload in ISSUE to the shape jira.el expects."
  (let* ((fields  (alist-get 'fields issue))
         (status  (or (alist-get 'status fields)
                      (alist-get 'status issue)))
         (name    (cond ((stringp status) status)
                        ((listp status) (alist-get 'name status))))
         (cat     (or (and (listp status) (alist-get 'statusCategory status))
                      (let ((color (or (task--acli-jira--get issue 'statusCategoryColor)
                                       (task--acli-jira--get issue 'statusColor))))
                        (when color `((colorName . ,color)))))))
    (cond
     ((and (listp status) (assoc 'name status)) status)
     (name `((name . ,name) (statusCategory . ,cat)))
     (t nil))))

(defun task--acli-jira-normalize-issue (issue)
  "Convert ISSUE returned by `acli jira' into the shape expected by task.el."
  (let* ((key       (task--acli-jira--get issue 'key))
         (summary   (task--acli-jira--get issue 'summary))
         (created   (task--acli-jira--get issue 'created))
         (updated   (task--acli-jira--get issue 'updated))
         (issuetype (task--acli-jira--build-type issue 'issuetype 'issuetypeName 'issuetypeIcon 'issuetypeId))
         (priority  (task--acli-jira--build-type issue 'priority 'priorityName 'priorityIcon 'priorityId))
         (status    (task--acli-jira--build-status issue))
         (fields nil))
    (when summary   (push (cons 'summary summary) fields))
    (when created   (push (cons 'created created) fields))
    (when updated   (push (cons 'updated updated) fields))
    (when issuetype (push issuetype fields))
    (when priority  (push priority fields))
    (when status    (push (cons 'status status) fields))
    `((key . ,key)
      (fields . ,(nreverse fields)))))

(defun task--acli-jira-search (jql max-results)
  "Search issues via `acli jira'. Return a vector of normalized issues."
  (let* ((cmd (append task-acli-jira-command
                      (list "search" "--jql" jql "--limit" (number-to-string max-results))
                      task-acli-jira-extra-args))
         (program (car cmd))
         (args (cdr cmd))
         (_ (unless (executable-find program)
              (error "Executable not found for acli jira backend: %s" program)))
         (output (with-temp-buffer
                   (let ((exit-code (apply #'process-file program nil (current-buffer) nil args)))
                     (unless (zerop exit-code)
                       (error "acli jira command failed (%s): %s" exit-code
                              (replace-regexp-in-string "\n\\'" "" (buffer-string))))
                     (buffer-string))))
         (data (json-parse-string output :object-type 'alist :array-type 'vector
                                  :null-object nil :false-object nil))
         (issues (or (alist-get 'issues data) data)))
    (vconcat (mapcar #'task--acli-jira-normalize-issue (append issues nil)))))

(defun task--fetch-issues (jql max-results)
  "Fetch issues according to `task-backend'."
  (pcase task-backend
    ('jira      (task--jira-api-search-sync jql max-results))
    ('acli-jira (task--acli-jira-search jql max-results))
    (_ (error "Unsupported task backend: %s" task-backend))))

(defun task--generate-branch-name (key text)
  "Make KEY and TEXT a valid branch name."
  (let* ((base-name (concat key "-" (downcase (replace-regexp-in-string "[^A-Za-z]+" "-" text))))
         (no-double-hyphens (replace-regexp-in-string "\\-\\-+" "-" base-name))
         (no-trailing-hyphen (replace-regexp-in-string "\\-$" "" no-double-hyphens)))
    no-trailing-hyphen))

(defun task--get-icon (url key)
  "Get image icon from URL with KEY for cache."
  (let* ((cache-file (expand-file-name key task-icon-cache-dir)))
    (unless (file-exists-p cache-file)
      (url-copy-file url cache-file t))
    (create-image cache-file nil nil :ascent 'center)))

(defun task--format-time (time-str)
  "Parse and format the TIME-STR with format string."
  (format-time-string "%Y-%m-%d %H:%M" (parse-iso8601-time-string time-str)))

(defun task--format-jira-status (status)
  "Format JIRA STATUS with appropriate color."
  (let* ((status-name (cdr (assoc 'name status)))
         (color-name (cdr (assoc 'colorName (cdr (assoc 'statusCategory status)))))
         (face (pcase color-name
                 ("green" 'success)
                 ("yellow" 'warning)
                 ("red" 'error)
                 ("blue-gray" 'font-lock-comment-face)
                 (_ 'default))))
    (propertize status-name 'face face)))

(defun task--jira-format-candidates (issues)
  "Format completion candidates for Jira ISSUES."
  (let ((table (make-hash-table :test 'equal)))
    (cl-loop for issue across issues
             do (let* ((key          (cdr (assoc 'key issue)))
                       (key-fmt      (propertize key 'face 'font-lock-number-face))
                       (fields       (cdr (assoc 'fields issue)))
                       (created      (cdr (assoc 'created fields)))
                       (updated      (cdr (assoc 'updated fields)))
                       (status       (cdr (assoc 'status fields)))
                       (status-fmt   (task--format-jira-status status))
                       (summary      (cdr (assoc 'summary fields)))
                       (summary-fmt  (propertize summary 'face 'font-lock-string-face))
                       (created-fmt  (task--format-time created))
                       (updated-fmt  (task--format-time updated))
                       (time-fmt     (propertize (format "[%s | %s]" created-fmt updated-fmt) 'face 'font-lock-comment-face))
                       (display (format
                                 "%-12s %-12s %s  %s"
                                 key-fmt
                                 status-fmt
                                 time-fmt
                                 summary-fmt)))
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
                 (task--fetch-issues jql task-jira-search-limit)))
         (table (task--jira-completion-table cands))
         (cand (completing-read "Select JIRA issue: " table)))
    (gethash cand cands)))

(defun task-create-branch-with-key-and-text (key text)
  "Use KEY and TEXT as name to create branch.

See `magit-branch-or-checkout'"
  (let* ((boc
          (magit-read-other-branch-or-commit
           "Checkout"
           nil
           (task--generate-branch-name key text)))
         (start-point
          (and (not (magit-commit-p boc))
               (magit-read-starting-point "Create and checkout branch" boc))))
    (when (string-match "\\`heads/\\(.+\\)" boc)
      (setq boc (match-string-no-properties 1 boc)))
    (if start-point
        (with-suppressed-warnings ((interactive-only magit-branch-and-checkout))
          (magit-branch-and-checkout boc start-point))
      (magit--checkout boc)
      (magit-refresh))))

(defun task-pull-remote-branch (&optional remote branch)
  "Pull BRANCH from REMOTE."
  (interactive
   (list (magit-read-remote "Select remote: ")
         (magit-read-starting-point "Branch")))
  (if (string= branch (magit-get-current-branch))
      (magit-pull-branch branch nil)
    (magit-fetch-refspec remote (format "%s:%s" branch branch) nil)))

;;;###autoload
(defun task-start-dev-work (issue &optional pull-first)
  "Start the development ISSUE with task.

If PULL-FIRST, will run task to pull the remote branch first."
  (interactive
   (list (task-jira-select-issue task-jira-default-jql)
         (or current-prefix-arg
             (y-or-n-p "Pull remote branch first? "))))
  (when pull-first
    (call-interactively #'task-pull-remote-branch))
  (let* ((key     (cdr (assoc 'key issue)))
         (fields  (cdr (assoc 'fields issue)))
         (summary (cdr (assoc 'summary fields))))
    (task-create-branch-with-key-and-text key summary)))

(provide 'task)
;;; task.el ends here
