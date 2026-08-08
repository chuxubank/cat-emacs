;; -*- lexical-binding: t; -*-

(use-package go-mode)

(use-package go-ts-mode
  :ensure nil
  :when EMACS29+
  :demand)

(use-package ob-go
  :demand
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(go . t)))

(use-package templ-ts-mode
  :font-role code-config)

(use-package go-template-ts-mode
  :font-role code-config
  :vc (:url "https://github.com/cat-emacs/go-template-ts-mode"))
