;; -*- lexical-binding: t; -*-

;; https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=feature/tree-sitter

(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4))

(use-package treesit-fold
  :delight
  :pin jcs-elpa
  :hook
  (after-init . global-treesit-fold-indicators-mode)
  (treesit-fold-mode . treesit-fold-line-comment-mode)
  :bind
  (:map treesit-fold-mode-map
        ([remap hs-hide-block] . treesit-fold-close)
        ([remap hs-show-block] . treesit-fold-open)
        ([remap hs-hide-all] . treesit-fold-close-all)
        ([remap hs-show-all] . treesit-fold-open-all)
        ([remap hs-toggle-hiding] . treesit-fold-toggle)
        ("C-c @ C-r" . treesit-fold-open-recursively))
  :custom
  (treesit-fold-line-count-show t)
  (treesit-fold-line-count-format " <%d lines> ")
  :custom-face
  (treesit-fold-replacement-face ,cat-hs-folded-face)
  :config
  (push '(import_list . (treesit-fold-range-seq 6 -1)) (alist-get 'kotlin-ts-mode treesit-fold-range-alist))
  (push '(import_declaration
          . (lambda (node offset)
              (treesit-fold-range-line-comment node offset "import ")))
        (alist-get 'java-ts-mode treesit-fold-range-alist)))

(use-package treesit-langs
  :hook ((text-mode prog-mode) . treesit-langs-major-mode-setup))

(use-package treesit-auto
  :disabled
  :hook (after-init . global-treesit-auto-mode)
  :custom
  (treesit-auto-install 'prompt)
  :config
  (dolist (lang '(cmake cpp bash))
    (setq treesit-auto-langs (delete lang treesit-auto-langs))))

(with-eval-after-load 'meow
  (use-package meow-tree-sitter
    :demand
    :config
    (meow-tree-sitter-register-defaults)))
