;; -*- lexical-binding: t; -*-

(use-package smartparens
  :defer t
  :config
  (require 'smartparens-config)
  (sp-with-modes '(org-mode)
    (sp-local-pair "$" "$")
    (sp-local-pair "\\[" "\\]"))
  (sp-pair "（" "）")
  (sp-pair "“" "”"))

(dolist (h '(org-mode-hook
	     mermaid-mode-hook))
  (add-hook h 'smartparens-mode))

(dolist (h '(c-mode-hook
	     lisp-mode-hook
	     lisp-interaction-mode-hook
	     emacs-lisp-mode-hook
	     js-mode-hook
	     bibtex-mode-hook))
  (add-hook h 'smartparens-strict-mode))
