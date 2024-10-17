;; -*- lexical-binding: t; -*-

(defun use-package-handler/:ignore-builtin (name _keyword args rest state)
  "Handler for the `:ignore-builtin' keyword in `use-package'.
It temporarily removes the built-in version of the specified package(s)
(or NAME if no argument is provided) from both `package--builtins' and
`package--builtin-versions'."
  (let* ((packages-to-ignore (or args (list name)))
         (body (use-package-process-keywords name rest state)))
    (let ((package--builtins
           (cl-reduce (lambda (builtins pkg)
                        (assq-delete-all pkg builtins))
                      packages-to-ignore
                      :initial-value package--builtins))
          (package--builtin-versions
           (cl-reduce (lambda (versions pkg)
                        (assq-delete-all pkg versions))
                      packages-to-ignore
                      :initial-value package--builtin-versions)))
      body)))

(defun use-package-normalize/:ignore-builtin (name _keyword args)
  "Normalize the arguments for `:ignore-builtin`.
Accepts either no argument, a single package symbol, or a list of package symbols."
  (cond
   ((null args) nil)
   ((symbolp args) (list args))
   ((and (listp args) (every #'symbolp args)) args)
   (t (use-package-error ":ignore-builtin takes either no argument, a package symbol, or a list of package symbols"))))

(eval-after-load 'use-package-core
  '(progn
     (add-to-list 'use-package-keywords ':ignore-builtin)
     (put 'use-package-handler/:ignore-builtin 'function-documentation "Ignore built-in package(s)")
     ;; No aliasing here to avoid cyclic references
     (setf (alist-get :ignore-builtin use-package-keywords) 'use-package-handler/:ignore-builtin)))

(provide 'ignore-builtin)
