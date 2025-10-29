;; -*- lexical-binding: t; -*-

(use-package lsp-bridge
  :delight " 󰘘"
  :vc (:url "https://github.com/manateelazycat/lsp-bridge")
  :hook
  (after-init . global-lsp-bridge-mode)
  :custom
  (lsp-bridge-enable-log init-file-debug)
  (codeium-bridge-folder (expand-file-name cat-codeium-dir))
  (acm-enable-codeium t)
  (acm-backend-codeium-api-key-path (expand-file-name "api_key" cat-codeium-dir)))

(use-package flymake-bridge
  :vc (:url "https://github.com/liuyinz/flymake-bridge")
  :hook (lsp-bridge-mode . flymake-bridge-setup))
