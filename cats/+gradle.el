;; -*- lexical-binding: t; -*-

(use-package groovy-mode
  :mode "\\.gradle\\'")

(use-package flymake-gradle
  :after flymake
  :config
  (flymake-gradle-setup))

(use-package flycheck-gradle
  :after flycheck (:any kotlin-mode java-mode)
  :config
  (flycheck-gradle-setup))
