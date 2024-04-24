;; -*- lexical-binding: t; -*-

(use-package lsp-mode
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp)

(use-package lsp-ui
  :hook lsp-mode
  :custom
  (lsp-headerline-breadcrumb-icons-enable nil))
