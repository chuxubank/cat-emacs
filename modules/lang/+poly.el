;; -*- lexical-binding: t; -*-

(use-package polymode
  :delight (polymode-minor-mode " "))

(add-to-list 'auto-mode-alist '("\\.plist\\'" . nxml-mode))

(use-package poly-any-jinja2
  :demand t
  :ensure nil)

(use-package poly-any-go-template
  :demand t
  :ensure nil
  :after (polymode go-template-ts-mode))
