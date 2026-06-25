;; -*- lexical-binding: t; -*-

(use-package magit
  :pin melpa-stable
  :commands #'magit-read-repository
  :custom
  (magit-blame-mode-lighter " ")
  (magit-repository-directories `(("~/Developer/" . 5)
                                  (,(getenv "XDG_CONFIG_HOME") . 3)
                                  (,(getenv "XDG_DATA_HOME") . 3)
                                  (,cat-pass-directory . 0)))
  (magit-diff-refine-hunk t)
  (magit-diff-fontify-hunk t)
  (magit-diff-refine-ignore-whitespace nil)
  (magit-diff-adjust-tab-width t)
  (magit-format-file-function #'magit-format-file-nerd-icons)
  :config
  (transient-append-suffix 'magit-branch #'magit-branch-rename
    '("w" "copy" magit-add-current-branch-to-kill-ring))
  (transient-append-suffix 'magit-pull #'magit-pull-branch
    '("b" "remote branch" magit-pull-remote-branch)))

(defun magit-add-current-branch-to-kill-ring ()
  "Show the current branch in the echo-area and add it to the `kill-ring'."
  (interactive)
  (let ((branch (magit-get-current-branch)))
    (if branch
        (progn (kill-new branch)
               (message "%s" branch))
      (user-error "There is not current branch"))))

(defun magit-pull-remote-branch (&optional remote branch)
  "Pull BRANCH from REMOTE."
  (interactive
   (list (magit-read-remote "Select remote: " "origin")
         (magit-read-starting-point "Branch")))
  (if (string= branch (magit-get-current-branch))
      (magit-pull-branch branch nil)
    (magit-fetch-refspec remote (format "%s:%s" branch branch) nil)))

(use-package magit-section
  :pin melpa-stable)

(use-package magit-difftastic
  :vc (:url "https://github.com/rschmukler/magit-difftastic")
  :after magit
  :demand t
  :custom
  (magit-difftastic-display "inline")
  :config
  (defvar cat-magit-difftastic-debug t
    "When non-nil, write magit-difftastic diagnostics to a debug buffer.")

  (defun cat-magit-difftastic-log (format-string &rest args)
    "Append a magit-difftastic diagnostic message."
    (when cat-magit-difftastic-debug
      (let ((message (apply #'format format-string args)))
        (with-current-buffer (get-buffer-create "*magit-difftastic-debug*")
          (goto-char (point-max))
          (insert (format-time-string "[%F %T.%3N] "))
          (insert (or (ignore-errors (abbreviate-file-name default-directory))
                      "<no default-directory>"))
          (insert " ")
          (insert message)
          (insert "\n")))))

  (defun cat-magit-difftastic--around (label orig &rest args)
    "Log elapsed time and errors around a magit-difftastic function."
    (let ((start (float-time)))
      (cat-magit-difftastic-log "%s start args=%S" label args)
      (condition-case err
          (let ((result (apply orig args)))
            (cat-magit-difftastic-log
             "%s done elapsed=%.3fs" label (- (float-time) start))
            result)
        (error
         (cat-magit-difftastic-log
          "%s error elapsed=%.3fs err=%S" label (- (float-time) start) err)
         (signal (car err) (cdr err))))))

  (defun cat-magit-difftastic--prewarm-advice (orig &rest args)
    (apply #'cat-magit-difftastic--around "prewarm" orig args))

  (defun cat-magit-difftastic--insert-file-sections-advice (orig files context)
    (cat-magit-difftastic-log
     "insert-file-sections files=%d context=%S files-list=%S"
     (length files) context files)
    (funcall orig files context))

  (defun cat-magit-difftastic--compute-align-col-advice (orig &rest args)
    (apply #'cat-magit-difftastic--around "compute-align-col" orig args))

  (defun cat-magit-difftastic--insert-chunks-advice (orig rendered file context)
    (cat-magit-difftastic-log
     "insert-chunks file=%s rendered-bytes=%d context=%S"
     file (length rendered) context)
    (funcall orig rendered file context))

  (defun cat-magit-difftastic--render-files-debug (jobs width)
    "Debug replacement for `magit-difftastic--render-files'."
    (require 'difftastic)
    (let* ((results (make-hash-table :test 'equal))
           (start (float-time))
           (env (difftastic--build-git-process-environment
                 width (list "--display" magit-difftastic-display))))
      (cat-magit-difftastic-log
       "render-files sync start jobs=%d width=%S display=%S env=%S files=%S"
       (length jobs) width magit-difftastic-display
       (seq-filter (lambda (s) (string-prefix-p "DFT_" s)) env)
       (mapcar #'car jobs))
      (pcase-dolist (`(,file . ,diff-args) jobs)
        (let* ((args (append diff-args (list "--" file)))
               (process-environment env)
               (file-start (float-time)))
          (cat-magit-difftastic-log
           "render sync start file=%s args=%S" file args)
          (condition-case err
              (with-temp-buffer
                (let ((exit (apply #'process-file "git" nil t nil args))
                      (bytes (buffer-size)))
                  (cat-magit-difftastic-log
                   "render sync finish file=%s exit=%S bytes=%d elapsed=%.3fs"
                   file exit bytes (- (float-time) file-start))
                  (when (equal exit 0)
                    (puthash file
                             (difftastic--ansi-color-apply (buffer-string))
                             results))))
            (error
             (cat-magit-difftastic-log
              "render sync error file=%s args=%S elapsed=%.3fs err=%S"
              file args (- (float-time) file-start) err)))))
      (cat-magit-difftastic-log
       "render-files sync done elapsed=%.3fs results=%d"
       (- (float-time) start) (hash-table-count results))
      results))

  (advice-add 'magit-difftastic--prewarm
              :around #'cat-magit-difftastic--prewarm-advice)
  (advice-add 'magit-difftastic--insert-file-sections
              :around #'cat-magit-difftastic--insert-file-sections-advice)
  (advice-add 'magit-difftastic--compute-align-col
              :around #'cat-magit-difftastic--compute-align-col-advice)
  (advice-add 'magit-difftastic--insert-chunks
              :around #'cat-magit-difftastic--insert-chunks-advice)
  (advice-add 'magit-difftastic--render-files
              :override #'cat-magit-difftastic--render-files-debug)
  (magit-difftastic-mode 1))

(use-package transient
  :ignore-builtin
  :pin melpa-stable)

(use-package magit-todos
  :demand t
  :after magit
  :config
  (magit-todos-mode 1))

(use-package glab)

(use-package gtea)

(use-package gogs)

(use-package buck)

(use-package forge
  :pin melpa-stable
  :demand t
  :after magit
  :config
  (when (eq HOST_ENV 'iv)
    (push '("git.infinityparadise.com"        ; GITHOST
            "git.infinityparadise.com/api/v4" ; APIHOST
            "git.infinityparadise.com"        ; WEBHOST and INSTANCE-ID
            forge-gitlab-repository)          ; CLASS
          forge-alist)))
