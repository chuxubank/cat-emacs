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
  (magit-diff-refine-ignore-whitespace nil)
  (magit-format-file-function #'magit-format-file-nerd-icons))

(use-package magit-section
  :pin melpa-stable)

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
  :after magit)
