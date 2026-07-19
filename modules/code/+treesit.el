;; -*- lexical-binding: t; -*-

;; https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=feature/tree-sitter

(require 'cl-lib)
(require 'seq)
(require 'treesit)

(defvar treesit-fold-range-alist)

(declare-function cat/org-src-kill-treesit-fontification-buffer nil)
(declare-function cat/treesit-langs-cleanup nil (&optional ignored))
(declare-function org-src-get-lang-mode-if-bound "org-src" (lang))
(declare-function treesit-fold-range-line-comment "treesit-fold"
                  (node offset prefix))
(declare-function treesit-langs--bin-dir "treesit-langs" ())

(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4))

(defvar-local cat/treesit-fold--language-cache nil
  "Alist mapping major modes to compatible parser languages in this buffer.")

(defconst cat/treesit-fold-mode-language-alist
  '((go-template-ts-mode . gotmpl)
    (sh-mode . bash))
  "Tree-sitter languages whose names cannot be inferred from their modes.")

(defun cat/treesit-fold--query-patterns ()
  "Return the fold query patterns for the current major mode."
  (when-let ((ranges (alist-get major-mode treesit-fold-range-alist)))
    (seq-mapcat (lambda (range) `((,(car range)) @name)) ranges)))

(defun cat/treesit-fold--language ()
  "Return the parser language compatible with the current fold rules.
This avoids using the first parser in polymode buffers, where parser order
does not necessarily match `major-mode'."
  (or (alist-get major-mode cat/treesit-fold--language-cache)
      (let* ((parsers (treesit-parser-list))
             (languages (mapcar #'treesit-parser-language parsers))
             (mode-name (symbol-name major-mode))
             (expected
              (or (alist-get major-mode cat/treesit-fold-mode-language-alist)
                  (when (string-match "\\`\\(.+\\)-ts-mode\\'" mode-name)
                    (intern (match-string 1 mode-name)))))
             (language
              (if expected
                  (and (memq expected languages) expected)
                (when-let ((patterns (cat/treesit-fold--query-patterns)))
                  (cl-loop for parser in parsers
                           for candidate = (treesit-parser-language parser)
                           when (ignore-errors
                                  (treesit-query-compile candidate patterns))
                           return candidate)))))
        (when language
          (push (cons major-mode language) cat/treesit-fold--language-cache)
          language))))

(defun cat/treesit-fold--root-node ()
  "Return the parser root node compatible with the current fold rules."
  (when-let ((language (cat/treesit-fold--language)))
    (ignore-errors (treesit-buffer-root-node language))))

(defun cat/treesit-fold-with-polymode-parser (function &rest args)
  "Call FUNCTION with ARGS using the parser matching the polymode span."
  (if (not (bound-and-true-p polymode-mode))
      (apply function args)
    (when-let ((root (cat/treesit-fold--root-node)))
      (let ((root-function (symbol-function 'treesit-buffer-root-node)))
        (cl-letf (((symbol-function 'treesit-buffer-root-node)
                   (lambda (&optional language tag)
                     (if language
                         (funcall root-function language tag)
                       root))))
          (apply function args))))))

(defun cat/treesit-fold-range-go-template-action (node offset)
  "Fold the body of Go-template action NODE using OFFSET."
  (let (begin end)
    (dotimes (index (treesit-node-child-count node))
      (let* ((child (treesit-node-child node index))
             (type (treesit-node-type child)))
        (when (and (not begin) (member type '("}}" "-}}")))
          (setq begin (treesit-node-end child)))
        (when (member type '("{{" "{{-"))
          (setq end (treesit-node-start child)))))
    (when (and begin end (<= begin end))
      (cons (+ begin (car offset)) (+ end (cdr offset))))))

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
  (dolist (function '(treesit-fold--foldable-node-at-pos
                      treesit-fold-close-all
                      treesit-fold-indicators-refresh))
    (unless (advice-member-p #'cat/treesit-fold-with-polymode-parser function)
      (advice-add function :around #'cat/treesit-fold-with-polymode-parser)))
  (setf (alist-get 'go-template-ts-mode treesit-fold-range-alist)
        (mapcar (lambda (type)
                  (cons type #'cat/treesit-fold-range-go-template-action))
                '(if_action range_action with_action
                  define_action block_action)))
  (push '(import_list . (treesit-fold-range-seq 6 -1)) (alist-get 'kotlin-ts-mode treesit-fold-range-alist))
  (push '(import_declaration
          . (lambda (node offset)
              (treesit-fold-range-line-comment node offset "import ")))
        (alist-get 'java-ts-mode treesit-fold-range-alist)))

(use-package treesit-langs
  :commands treesit-langs-major-mode-setup
  :init
  (+add-to-list-multi 'major-mode-remap-alist
                      '(c++-mode        . c++-ts-mode)
                      '(c-mode          . c-ts-mode)
                      '(c-or-c++-mode   . c-or-c++-ts-mode)
                      '(conf-toml-mode  . toml-ts-mode)
                      '(js-mode         . js-ts-mode)
                      '(kotlin-mode     . kotlin-ts-mode)
                      '(rust-mode       . rust-ts-mode)
                      '(swift-mode      . swift-ts-mode)
                      '(sh-mode         . bash-ts-mode)
                      '(typescript-mode . typescript-ts-mode))
  :config
  (defun cat/treesit-langs-cleanup (&optional _)
    (interactive)
    (delete-directory (treesit-langs--bin-dir) t))
  (advice-add 'treesit-langs-install-grammars :before #'cat/treesit-langs-cleanup))

(use-package treesit-auto
  :disabled
  :hook (after-init . global-treesit-auto-mode)
  :custom
  (treesit-auto-install 'prompt)
  :config
  (dolist (lang '(cmake cpp bash))
    (setq treesit-auto-langs (delete lang treesit-auto-langs))))

(with-eval-after-load 'org-src
  (defun cat/org-src-kill-treesit-fontification-buffer (lang &rest _)
    "Kill reusable fontification buffer for tree-sitter modes.
`org-src-font-lock-fontify-block' reuses a single buffer and skips
mode re-initialization when already active, breaking tree-sitter
fontification on subsequent calls."
    (when-let* ((mode (org-src-get-lang-mode-if-bound lang))
                (buf (get-buffer (format " *org-src-fontification:%s*" mode)))
                ((with-current-buffer buf (treesit-parser-list))))
      (kill-buffer buf)))
  (advice-add 'org-src-font-lock-fontify-block :before
              #'cat/org-src-kill-treesit-fontification-buffer))

(with-eval-after-load 'meow
  (use-package meow-tree-sitter
    :demand
    :config
    (meow-tree-sitter-register-defaults)))
