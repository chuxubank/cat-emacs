;; -*- lexical-binding: t; -*-

;; https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=feature/tree-sitter

(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4))

(use-package treesit-fold
  :disabled
  :hook (after-init . global-treesit-fold-mode)
  :pin jcs-elpa)

(use-package treesit-auto
  :hook (after-init . global-treesit-auto-mode)
  :custom
  (treesit-auto-install 'prompt)
  :config
  (dolist (lang '(cmake cpp bash))
    (setq treesit-auto-langs (delete lang treesit-auto-langs)))
  (setq treesit-language-source-alist (treesit-auto--build-treesit-source-alist)))

(with-eval-after-load 'meow
  (use-package meow-tree-sitter
    :demand
    :config
    (meow-tree-sitter-register-defaults)))
