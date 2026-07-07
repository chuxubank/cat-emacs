;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)

(defun use-package-handler/:cat (name _keyword args rest state)
  "Handler for the `:cat' keyword in `use-package'.
It skips the package declaration unless ARGS evaluates to non-nil."
  (when (eval args lexical-binding)
    (use-package-process-keywords name rest state)))

(defun use-package-normalize/:cat (name _keyword args)
  "Normalize the arguments for `:cat'.
With no argument, use NAME as the Cat feature.  With one argument, t and nil are
direct boolean values, a symbol names a Cat feature, and a list is treated as an
expression."
  (if (null args)
      `(catp! ,name)
    (use-package-only-one ":cat" args
      (lambda (_label arg)
        (cond
         ((memq arg '(t nil)) arg)
         ((symbolp arg) `(catp! ,arg))
         (t arg))))))

(defun cat--use-package-position-cat ()
  "Ensure `:cat' is processed before `:ensure' in `use-package'.

`:ensure' unconditionally pushes package-install code regardless of the
body returned by inner keywords, so `:cat' can only prevent installation
when it runs as an outer (earlier) keyword than `:ensure'.  Simply
prepending `:cat' via `add-to-list' is not enough because
`use-package-ensure' later prepends `:ensure' back to the front."
  (setq use-package-keywords (delq :cat use-package-keywords))
  (let ((pos (or (cl-position :ensure use-package-keywords) 0)))
    (setq use-package-keywords
          (append (seq-take use-package-keywords pos)
                  (list :cat)
                  (seq-drop use-package-keywords pos)))))

(eval-after-load 'use-package-core
  '(progn
     (put 'use-package-handler/:cat 'function-documentation "Skip package unless its Cat expression is non-nil")
     (cat--use-package-position-cat)))

;; `use-package-ensure' prepends `:ensure' to the front of the keyword list
;; when it loads, so re-position `:cat' afterwards to keep it ahead of
;; `:ensure'.  This is what actually prevents skipped packages from being
;; installed when `use-package-always-ensure' is non-nil.
(eval-after-load 'use-package-ensure
  '(cat--use-package-position-cat))

(provide 'cat-use-package)
