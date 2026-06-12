;; -*- lexical-binding: t; -*-

(use-package dart-mode)

(when (package-installed-p 'lsp)
  (use-package lsp-dart))
