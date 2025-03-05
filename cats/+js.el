;; -*- lexical-binding: t; -*-

(use-package js
  :ensure nil
  :ensure-system-package
  (typescript-language-server . "bun add -g typescript-language-server")
  (vscode-json-languageserver . "bun add -g vscode-json-languageserver")
  :custom
  (js-indent-level 2))

(defun cat-is-node-package-lock-buffer ()
  (string-match-p "package-lock.json" (buffer-name)))
