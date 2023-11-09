;; -*- lexical-binding: t; -*-

;; https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=feature/tree-sitter

(unless EMACS29+
  (use-package tree-sitter
    :ensure EMACS29+
    :hook
    (after-init . global-tree-sitter-mode)
    (tree-sitter-after-on . tree-sitter-hl-mode))

  (use-package tree-sitter-langs
    :defer t)

  (use-package ts-fold
    :vc (:url "https://github.com/emacs-tree-sitter/ts-fold" :rev :newest)
    :hook
    (after-init . global-ts-fold-indicators-mode)))

(use-package treesit-auto
  :when EMACS29+
  :hook (after-init . global-treesit-auto-mode)
  :config
  (setq treesit-font-lock-level 4
        treesit-language-source-alist (treesit-auto--build-treesit-source-alist)))
