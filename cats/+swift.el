;; -*- lexical-binding: t; -*-

(use-package swift-mode
  :unless EMACS29+)

(use-package swift-ts-mode
  :when EMACS29+)

(use-package ob-swiftui
  :demand t
  :after org
  :config
  (ob-swiftui-setup))
