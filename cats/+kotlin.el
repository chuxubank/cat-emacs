;; -*- lexical-binding: t; -*-

(use-package kotlin-mode
  :defer t)

(use-package flycheck-kotlin
  :after flycheck
  :config
  (flycheck-kotlin-setup))
