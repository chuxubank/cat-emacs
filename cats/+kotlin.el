;; -*- lexical-binding: t; -*-

(use-package kotlin-mode
  :defer t)

(use-package flycheck-kotlin
  :after flycheck kotlin-mode
  :config
  (flycheck-kotlin-setup))
