;; -*- lexical-binding: t; -*-

;; https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=feature/tree-sitter

(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4))

(+add-to-list-multi 'major-mode-remap-alist
                    '(c++-mode        . c++-ts-mode)
                    '(c-mode          . c-ts-mode)
                    '(c-or-c++-mode   . c-or-c++-ts-mode)
                    '(conf-toml-mode  . toml-ts-mode)
                    '(js-mode         . js-ts-mode)
                    '(kotlin-mode     . kotlin-ts-mode)
                    '(swift-mode      . swift-ts-mode)
                    '(sh-mode         . bash-ts-mode)
                    '(typescript-mode . typescript-ts-mode))

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
  :commands treesit-langs-major-mode-setup)

(defun +treesit-langs-cleanup (&optional _)
  (interactive)
  (delete-directory (treesit-langs--bin-dir) t))

(advice-add 'treesit-langs-install-grammars :before #'+treesit-langs-cleanup)

(use-package treesit-auto
  :disabled
  :hook (after-init . global-treesit-auto-mode)
  :custom
  (treesit-auto-install 'prompt)
  :config
  (dolist (lang '(cmake cpp bash))
    (setq treesit-auto-langs (delete lang treesit-auto-langs))))

(with-eval-after-load 'org-src
  (defun +org-src-kill-treesit-fontification-buffer (lang &rest _)
    "Kill reusable fontification buffer for tree-sitter modes.
`org-src-font-lock-fontify-block' reuses a single buffer and skips
mode re-initialization when already active, breaking tree-sitter
fontification on subsequent calls."
    (when-let* ((mode (org-src-get-lang-mode-if-bound lang))
                (buf (get-buffer (format " *org-src-fontification:%s*" mode)))
                ((with-current-buffer buf (treesit-parser-list))))
      (kill-buffer buf)))
  (advice-add 'org-src-font-lock-fontify-block :before
              #'+org-src-kill-treesit-fontification-buffer))

(with-eval-after-load 'meow
  (use-package meow-tree-sitter
    :demand
    :config
    (meow-tree-sitter-register-defaults)))
