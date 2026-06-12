;; -*- lexical-binding: t; -*-

(use-package smartparens
  :delight '(:eval (if smartparens-strict-mode " 󱃗" " 󰅲"))
  :hook ((prog-mode text-mode) . smartparens-mode)
  :config
  (require 'smartparens-config)
  (sp-pair "（" "）")
  (sp-pair "“" "”")
  (sp-with-modes '(org-mode)
    (sp-local-pair "$" "$")
    (sp-local-pair "\\[" "\\]")))
