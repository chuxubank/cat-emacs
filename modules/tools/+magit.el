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
  :vc (:url "https://github.com/chuxubank/magit-difftastic")
  :after magit
  :demand t
  :custom
  (magit-difftastic-display "inline")
  :config
  (transient-append-suffix 'magit-diff-refresh '(1 1 -1)
    '("D" "difftastic" magit-difftastic-mode)))

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
