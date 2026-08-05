;; -*- lexical-binding: t; -*-

(cat-register-font-rule 'objc '(:modes objc-mode :font code-apple))

(use-package swift-mode
  :cat-font code-apple
  :unless EMACS29+)

(use-package swift-ts-mode
  :cat-font code-apple
  :when EMACS29+)

(use-package ob-swiftui
  :demand t
  :after org
  :config
  (ob-swiftui-setup))
