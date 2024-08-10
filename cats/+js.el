;; -*- lexical-binding: t; -*-

(use-package js
  :ensure nil
  :ensure-system-package
  (node . "pnpm env use -g lts")
  (typescript-language-server . "pnpm add -g typescript-language-server")
  :custom
  (js-indent-level 2))

(use-package json-mode
  :bind
  (:map json-mode-map
        ("C-c C-w" . json-mode-kill-path)))

(defun cat-is-node-package-lock-buffer ()
  (string-match-p "package-lock.json" (buffer-name)))
