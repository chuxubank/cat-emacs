;; -*- lexical-binding: t; -*-

(use-package polymode
  :delight (polymode-minor-mode " "))

(add-to-list 'auto-mode-alist '("\\.plist\\'" . nxml-mode))

(use-package poly-any-jinja2
  :vc (:url "https://github.com/chuxubank/poly-any-template"
            :main-file "poly-any-jinja2.el")
  :custom
  (poly-any-jinja2-lighter "  "))

(use-package poly-any-go-template
  :vc (:url "https://github.com/chuxubank/poly-any-template"
            :main-file "poly-any-go-template.el")
  :custom
  (poly-any-go-template-lighter "  "))
