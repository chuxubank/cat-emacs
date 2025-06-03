;; -*- lexical-binding: t; -*-

(use-package swift-mode)

(use-package swift-ts-mode)

(use-package ob-swiftui
  :demand t
  :after org
  :config
  (ob-swiftui-setup))
