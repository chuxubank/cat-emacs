(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c p l")
  :hook ((kotlin-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)
