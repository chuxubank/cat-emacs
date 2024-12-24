;; -*- lexical-binding: t; -*-

(use-package magit
  :pin nongnu
  :commands #'magit-read-repository
  :custom
  (magit-blame-mode-lighter " ")
  (magit-repository-directories '(("~/Developer/" . 5)
                                  ("~/.emacs.d/". 0)
                                  ("~/org/" . 0)
                                  ("~/org-roam/" . 0)
                                  ("~/.local/share/chezmoi/" . 0)
                                  ("~/.password-store/" . 0)))
  (magit-diff-refine-hunk t)
  (magit-diff-refine-ignore-whitespace nil))

(use-package forge
  :pin melpa-stable
  :demand t
  :after magit)
