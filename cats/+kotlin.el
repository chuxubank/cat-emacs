;; -*- lexical-binding: t; -*-

(use-package kotlin-ts-mode
  :defer t)

(use-package flycheck-kotlin
  :after flycheck kotlin-ts-mode
  :config
  (flycheck-kotlin-setup))
