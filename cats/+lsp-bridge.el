;; -*- lexical-binding: t; -*-

(straight-use-package '(markdown-mode :type built-in))

(use-package lsp-bridge
  :straight (lsp-bridge :host github :repo "manateelazycat/lsp-bridge"
            :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
            :build (:not compile))
  :hook
  (after-init . global-lsp-bridge-mode)
  :custom
  (codeium-bridge-folder (expand-file-name cat-codeium-dir))
  (acm-enable-codeium t)
  (acm-backend-codeium-api-key-path cat-codeium-api-key-file)
  :config
  (global-corfu-mode -1))

(use-package flymake-bridge
  :straight (flymake-bridge :host github :repo "liuyinz/flymake-bridge")
  :hook (lsp-bridge-mode . flymake-bridge-setup))
