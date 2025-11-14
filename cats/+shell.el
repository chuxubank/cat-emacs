;; -*- lexical-binding: t; -*-


(use-package sh-script
  :ensure nil
  :init
  (add-to-list 'major-mode-remap-alist
               '(sh-mode . bash-ts-mode))
  :custom
  (sh-basic-offset 4))

(use-package powershell)

(use-package ob-powershell
  :demand t
  :after org)
