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
  :config
  (defun treesit-fold--continuous-node-prefix (node prefix next)
    "Iterate through node starting from NODE and compare node-text to PREFIX;
then return the last iterated node.

Argument NEXT is a boolean type.  If non-nil iterate forward; otherwise iterate
in backward direction."
    (let* ((iter-node node) (last-node node)
           (last-line (car (treesit-fold--node-start-position node))) line text break
           (line-range 1) (last-line-range 1) max-line-range
           (indentation (treesit-fold--indentation (treesit-node-start iter-node)))
           next-indentation)
      (while (and iter-node (not break))
        (setq text (string-trim (treesit-node-text iter-node))
              line (car (treesit-fold--node-start-position iter-node))
              line-range (1+ (treesit-fold--count-matches "\n" text))
              max-line-range (max line-range last-line-range)
              next-indentation (treesit-fold--indentation (treesit-node-start iter-node)))
        (if (and (treesit-fold--in-range-p line (- last-line max-line-range) (+ last-line max-line-range))
                 (string-prefix-p prefix text)
                 (= indentation next-indentation))
            (setq last-node iter-node last-line line
                  last-line-range (1+ (treesit-fold--count-matches "\n" text)))
          (setq break t))
        (setq iter-node (treesit-fold--next-prev-node-skip-newline iter-node next)))
      last-node))
  (push '(import_list . (treesit-fold-range-seq 6 -1)) (alist-get 'kotlin-ts-mode treesit-fold-range-alist))
  (push '(import_declaration
          . (lambda (node offset)
              (treesit-fold-range-line-comment node offset "import "))) (alist-get 'java-ts-mode treesit-fold-range-alist)))

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
