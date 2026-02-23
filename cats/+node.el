;; -*- lexical-binding: t; -*-

(use-package js
  :ensure nil
  :custom
  (js-indent-level 2))

(use-package json-ts-mode
  :ensure nil
  :ensure-system-package
  (vscode-json-languageserver . "bun add -g vscode-json-languageserver")
  :when EMACS29+
  :demand)

(use-package typescript-ts-mode
  :ensure nil
  :ensure-system-package
  (typescript-language-server . "bun add -g typescript-language-server")
  :when EMACS29+
  :demand)

(use-package ob-typescript
  :demand
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(typescript . t)))

(defun cat-node-package-lock-buffer-p ()
  (string-match-p "package-lock.json" (buffer-name)))
