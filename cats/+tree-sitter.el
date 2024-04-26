;; -*- lexical-binding: t; -*-

;; https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=feature/tree-sitter

(setq treesit-font-lock-level 4)

(use-package tree-sitter
  :delight " "
  :hook
  (after-init . global-tree-sitter-mode)
  (tree-sitter-after-on . tree-sitter-hl-mode))

(use-package tree-sitter-langs)

(use-package ts-docstr
  :pin jcs-elpa)

(defun ts-fold-range-same (node offset prefix)
  "Define fold range for same tag with PREFIX.

For arguments NODE and OFFSET, see function `ts-fold-range-seq' for
more information."
  (save-excursion
    (when-let* ((first-node (ts-fold--continuous-node-prefix node prefix nil))
                (last-node (ts-fold--continuous-node-prefix node prefix t))
                (prefix-len (length prefix))
                (beg (+ (tsc-node-start-position first-node) prefix-len))
                (end (tsc-node-end-position last-node)))
      (ts-fold--cons-add (cons beg end) offset))))

(use-package ts-fold
  :pin jcs-elpa
  :delight " "
  :hook
  (after-init . global-ts-fold-indicators-mode)
  :bind
  (:map prog-mode-map
        ("C-." . ts-fold-toggle))
  :config
  (push '(import_list . (ts-fold-range-seq 6 -1)) (alist-get 'kotlin-mode ts-fold-range-alist))
  (push '(import_declaration
          . (lambda (node offset)
              (ts-fold-range-same node offset "import "))) (alist-get 'java-mode ts-fold-range-alist)))

(use-package treesit-auto
  :disabled
  :when EMACS29+
  :hook (after-init . global-treesit-auto-mode)
  :custom
  (treesit-auto-install 'prompt)
  :config
  (dolist (lang '(cmake cpp bash))
    (setq treesit-auto-langs (delete lang treesit-auto-langs)))
  (setq treesit-language-source-alist (treesit-auto--build-treesit-source-alist)))
