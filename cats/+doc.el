;; -*- lexical-binding: t; -*-

(use-package eldoc-box
  :hook (eldoc-mode . eldoc-box-hover-at-point-mode))

(setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

(defun cat-elisp-eldoc-funcall (callback &rest _)
  "Document function call at point by calling CALLBACK.
Intended for `eldoc-documentation-functions' (which see)."
  (let* ((func (function-called-at-point))
         (docstring (cat-elisp-function-doc func)))
    (when docstring
      (funcall callback docstring
               :face 'font-lock-doc-face))))

(defun cat-elisp-function-doc (function)
  "Return documentation string for FUNCTION."
  (when function
    (with-temp-buffer
      (let ((standard-output (current-buffer))
            (help-xref-following t))
        (prin1 function)
        (princ " is ")
        (describe-function-1 function)
        (buffer-string)))))

(advice-add 'elisp-eldoc-funcall :before-while #'cat-elisp-eldoc-funcall)

(advice-add 'elisp-eldoc-var-docstring :override #'elisp-eldoc-var-docstring-with-value)
