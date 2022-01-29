;; -*- lexical-binding: t; -*-

(use-package pass
  :defer t
  :bind
  ("C-c p p" . #'pass)
  :custom
  (password-store-password-length 16))

(use-package pinentry
  :after pass
  :config
  (pinentry-start))
