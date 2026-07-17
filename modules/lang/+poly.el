;; -*- lexical-binding: t; -*-

(use-package polymode
  :delight (polymode-minor-mode " "))

(add-to-list 'auto-mode-alist '("\\.plist\\'" . nxml-mode))

(use-package poly-any-jinja2
  :demand t
  :vc (poly-any-jinja2
       :url "https://github.com/chuxubank/poly-any-template"
       :main-file "poly-any-jinja2.el"))

(use-package poly-any-go-template
  :demand t
  :vc (poly-any-go-template
       :url "https://github.com/chuxubank/poly-any-template"
       :main-file "poly-any-go-template.el"))
