;; -*- lexical-binding: t; -*-

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

(eval-after-load 'use-package-core
  '(progn
     (add-to-list 'use-package-keywords ':cat)
     (put 'use-package-handler/:cat 'function-documentation "Skip package unless its Cat expression is non-nil")
     (setf (alist-get :cat use-package-keywords) 'use-package-handler/:cat)))

(provide 'cat-use-package)
