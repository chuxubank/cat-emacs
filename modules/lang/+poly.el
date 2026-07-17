;; -*- lexical-binding: t; -*-

(use-package polymode
  :delight (polymode-minor-mode " "))

(add-to-list 'auto-mode-alist '("\\.plist\\'" . nxml-mode))

(use-package poly-any-template
  :demand t
  :vc (:url "https://github.com/chuxubank/poly-any-template"))
