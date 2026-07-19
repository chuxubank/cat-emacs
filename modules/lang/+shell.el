;; -*- lexical-binding: t; -*-

(declare-function zsh-ts-mode--redirect-bash-ts-mode "zsh-ts-mode")

(use-package sh-script
  :ensure nil
  :custom
  (sh-basic-offset 4))

(use-package zsh-ts-mode
  :vc (:url "https://github.com/zardoz03/zsh-ts-mode")
  :mode (("\\(?:^\\|/\\)\\.z\\(?:profile\\|shenv\\|shrc\\|login\\|logout\\)\\'"
          . zsh-ts-mode)
         ("\\.zsh\\'" . zsh-ts-mode))
  :init
  (add-to-list 'treesit-language-source-alist
               '(zsh "https://github.com/tree-sitter-grammars/tree-sitter-zsh"))
  :config
  (unless (treesit-language-available-p 'zsh)
    (treesit-install-language-grammar 'zsh))
  (advice-remove 'zsh-ts-mode #'zsh-ts-mode--redirect-bash-ts-mode))

(use-package powershell)

(use-package ob-powershell
  :demand t
  :after org)
