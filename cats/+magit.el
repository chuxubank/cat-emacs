;; -*- lexical-binding: t; -*-

(use-package magit
  :commands #'magit-read-repository
  :custom
  (magit-blame-mode-lighter " ")
  (magit-repository-directories '(("~/Developer/" . 5)
                                  ("~/.emacs.d/". 0)
                                  ("~/org/" . 0)
                                  ("~/org-roam/" . 0)
                                  ("~/.local/share/chezmoi/" . 0)))
  (magit-diff-refine-hunk t)
  (magit-diff-refine-ignore-whitespace nil)
  :config
  (setq magit-stash-read-message-function #'magit-stash-read-message-traditional))

(use-package forge
  :demand t
  :after magit)
