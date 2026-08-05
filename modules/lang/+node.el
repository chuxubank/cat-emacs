;; -*- lexical-binding: t; -*-

(cat-register-font-rule
 'typescript-mode
 '(:modes typescript-mode :font code-jvm))

(use-package js
  :ensure nil
  :cat-font (code-jvm :modes js-base-mode)
  :custom
  (js-indent-level 2))

(use-package json-ts-mode
  :ensure nil
  :cat-font (code-config :modes (json-mode json-ts-mode))
  :ensure-system-package
  (vscode-json-languageserver . "bun add -g vscode-json-languageserver")
  :when EMACS29+
  :demand)

(use-package typescript-ts-mode
  :ensure nil
  :cat-font (code-jvm :modes typescript-ts-base-mode)
  :ensure-system-package
  (typescript-language-server . "bun add -g typescript-language-server")
  :when EMACS29+
  :demand)

(use-package ob-typescript
  :demand
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(typescript . t)))

(defun cat/node-package-lock-buffer-p ()
  (string-match-p "package-lock.json" (buffer-name)))
