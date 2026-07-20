;; -*- lexical-binding: t; -*-

(use-package polymode
  :delight (polymode-minor-mode " "))

(add-to-list 'auto-mode-alist '("\\.plist\\'" . nxml-mode))

(use-package jinja2-ts-mode
  :vc (:url "https://github.com/chuxubank/jinja2-ts-mode"))

(use-package poly-any-template
  :vc (:url "https://github.com/chuxubank/poly-any-template"
            :lisp-dir "lisp/shared"))

(use-package poly-any-jinja2
  :vc (:url "https://github.com/chuxubank/poly-any-template"
            :lisp-dir "lisp/jinja2")
  :custom
  (poly-any-jinja2-lighter "  "))

(use-package poly-any-go-template
  :vc (:url "https://github.com/chuxubank/poly-any-template"
            :lisp-dir "lisp/go-template")
  :custom
  (poly-any-go-template-lighter "  "))
