;; -*- lexical-binding: t; -*-

(use-package magit
  :pin melpa-stable
  :commands #'magit-read-repository
  :custom
  (magit-blame-mode-lighter " ")
  (magit-repository-directories `(("~/Developer/" . 5)
                                  (,(getenv "XDG_CONFIG_HOME") . 3)
                                  (,(getenv "XDG_DATA_HOME") . 3)))
  (magit-diff-refine-hunk t)
  (magit-diff-refine-ignore-whitespace nil))

(use-package magit-file-icons
  :hook
  (after-init . magit-file-icons-mode))

(use-package magit-todos
  :demand t
  :after magit
  :config
  (magit-todos-mode 1))

(use-package forge
  :pin melpa-stable
  :demand t
  :after magit)
