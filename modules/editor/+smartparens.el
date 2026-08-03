;; -*- lexical-binding: t; -*-

(use-package smartparens
  :delight (smartparens-mode
            (:eval (concat " " (+with-icon (if smartparens-strict-mode
                                                "nf-md-code_parentheses_box"
                                              "nf-md-code_parentheses")))))
  :hook ((prog-mode text-mode) . smartparens-mode)
  :config
  (require 'smartparens-config)
  (sp-pair "（" "）")
  (sp-pair "“" "”")
  (sp-with-modes '(org-mode)
    (sp-local-pair "$" "$")
    (sp-local-pair "\\[" "\\]")))
