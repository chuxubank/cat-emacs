;; -*- lexical-binding: t; -*-

(use-package groovy-mode
  :mode "\\.gradle\\'")

(use-package flymake-gradle
  :hook (flymake-mode . flymake-gradle-setup))

(use-package flycheck-gradle
  :hook (flycheck-mode . flycheck-gradle-setup))
