;; -*- lexical-binding: t; -*-

(use-package polymode
  :delight (polymode-minor-mode
            (:eval (+with-icon "nf-cod-layers" " "))))

(add-to-list 'auto-mode-alist '("\\.plist\\'" . nxml-mode))

(use-package jinja2-ts-mode
  :cat-font code-config
  :vc (:url "https://github.com/cat-emacs/jinja2-ts-mode"))

(use-package poly-any-template
  :vc (:url "https://github.com/cat-emacs/poly-any-template"
            :lisp-dir "lisp/shared"))

(use-package poly-any-template-indent-bars
  :vc (:url "https://github.com/cat-emacs/poly-any-template"
            :lisp-dir "lisp/indent-bars")
  :hook
  (poly-any-template-after-activate . poly-any-template-indent-bars-mode))

(use-package poly-any-jinja2
  :vc (:url "https://github.com/cat-emacs/poly-any-template"
            :lisp-dir "lisp/jinja2")
  :custom
  (poly-any-jinja2-lighter
   (concat (+with-icon "nf-seti-jinja" " " " ")
           (+with-icon "nf-cod-layers_active"))))

(use-package poly-any-go-template
  :vc (:url "https://github.com/cat-emacs/poly-any-template"
            :lisp-dir "lisp/go-template")
  :custom
  (poly-any-go-template-lighter
   (concat (+with-icon "nf-dev-go" " " " ")
           (+with-icon "nf-cod-layers_active"))))
