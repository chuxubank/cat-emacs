;; -*- lexical-binding: t; -*-

(use-package swift-mode
  :mode-hydra
  (("LSP"
    (("e" eglot-hydra/body "eglot")))))

(use-package swift-ts-mode
  :mode-hydra
  (("LSP"
    (("e" eglot-hydra/body "eglot")))))

(use-package ob-swiftui
  :demand t
  :after org
  :config
  (ob-swiftui-setup))
