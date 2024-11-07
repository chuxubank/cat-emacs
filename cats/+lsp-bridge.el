;; -*- lexical-binding: t; -*-

(use-package lsp-bridge
  :delight " 󰘘"
  :vc (lsp-bridge
       :url "https://github.com/manateelazycat/lsp-bridge"
       :rev :newest)
  :hook
  (after-init . global-lsp-bridge-mode)
  :custom
  (codeium-bridge-folder (expand-file-name cat-codeium-dir))
  (acm-enable-codeium t)
  (acm-backend-codeium-api-key-path cat-codeium-api-key-file))

(use-package flymake-bridge
  :vc (flymake-bridge
       :url "https://github.com/liuyinz/flymake-bridge"
       :rev :newest)
  :hook (lsp-bridge-mode . flymake-bridge-setup))
