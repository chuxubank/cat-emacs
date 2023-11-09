;; -*- lexical-binding: t; -*-

(use-package kotlin-ts-mode
  :ensure-system-package
  (kotlin-language-server . kotlin-language-server)
  :defer t)

(use-package flycheck-kotlin
  :after flycheck kotlin-ts-mode
  :config
  (flycheck-kotlin-setup))
