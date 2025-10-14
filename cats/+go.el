;; -*- lexical-binding: t; -*-

(use-package go-mode)

(use-package ob-go
  :demand
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(go . t)))

(use-package templ-ts-mode)

(use-package go-template-mode
  :ensure nil
  :mode "\\.tmpl\\'")
