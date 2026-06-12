;; -*- lexical-binding: t; -*-


(use-package sh-script
  :ensure nil
  :custom
  (sh-basic-offset 4))

(use-package powershell)

(use-package ob-powershell
  :demand t
  :after org)
