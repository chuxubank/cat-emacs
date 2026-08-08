;; -*- lexical-binding: t; -*-

(prosody-register 'objc '(:modes objc-mode :font code-apple))

(use-package swift-mode
  :font-role code-apple
  :unless EMACS29+)

(use-package swift-ts-mode
  :font-role code-apple
  :when EMACS29+)

(use-package ob-swiftui
  :demand t
  :after org
  :config
  (ob-swiftui-setup))
