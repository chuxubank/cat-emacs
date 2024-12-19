;; -*- lexical-binding: t; -*-

(use-package js
  :ensure nil
  :ensure-system-package
  (typescript-language-server . "bun add -g typescript-language-server")
  :custom
  (js-indent-level 2))

(use-package json-mode
  :ensure-system-package
  (vscode-json-languageserver . "bun add -g vscode-json-languageserver")
  :bind
  (:map json-mode-map
        ("C-c C-w" . json-mode-kill-path)))

(defun cat-is-node-package-lock-buffer ()
  (string-match-p "package-lock.json" (buffer-name)))
