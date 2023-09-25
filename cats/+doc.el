;; -*- lexical-binding: t; -*-

(use-package eldoc-box
  :hook (eldoc-mode . eldoc-box-hover-at-point-mode)
  :custom
  (eldoc-box-lighter nil)
  (eldoc-minor-mode-string
   '(" Eldoc" (:eval (cond (eldoc-box-hover-at-point-mode "/bhp")
                           (eldoc-box-hover-mode "/bh")
                           (t nil)))))
  :config
  (put 'eldoc-minor-mode-string 'risky-local-variable t))

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

(when EMACS29+
  ;; (advice-add 'elisp-eldoc-funcall :before-while #'cat-elisp-eldoc-funcall)
  (advice-add 'elisp-eldoc-var-docstring :override #'elisp-eldoc-var-docstring-with-value))

