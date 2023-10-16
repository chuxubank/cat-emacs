;; -*- lexical-binding: t; -*-

(use-package lsp-mode
  :custom
  (lsp-session-file (concat cat-etc-dir "lsp-session"))
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-headerline-breadcrumb-icons-enable nil))
