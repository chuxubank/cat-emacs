;; -*- lexical-binding: t; -*-

(add-to-list 'major-mode-remap-alist
             '(sh-mode . bash-ts-mode))

(use-package powershell)

(use-package ob-powershell
  :demand t
  :after org)
