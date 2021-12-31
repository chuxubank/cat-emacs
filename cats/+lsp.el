;; -*- lexical-binding: t; -*-

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c C-l"
	lsp-session-file (concat cat-etc-dir "lsp-session"))
  :hook ((kotlin-mode . lsp)
	 (dart-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode)

(setq treemacs-persist-file (concat cat-cache-dir "treemacs"))
