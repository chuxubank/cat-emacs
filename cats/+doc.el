;; -*- lexical-binding: t; -*-

(setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
(put 'eldoc-minor-mode-string 'risky-local-variable t)

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

(use-package eldoc-box
  :when (or (daemonp)
            (display-graphic-p))
  :hook (eldoc-mode . eldoc-box-hover-at-point-mode)
  :custom
  (eldoc-box-lighter nil)
  (eldoc-minor-mode-string
   '(" " (:eval (cond (eldoc-box-hover-at-point-mode "󰷉")
                      (eldoc-box-hover-mode "󱔘")
                      (t "󰧮"))))))

(use-package eldoc-toml
  :delight
  :hook ((conf-toml-mode
          toml-ts-mode). eldoc-toml-mode))

(use-package devdocs)

(defvar-keymap cat-dev-doc-map
  :doc "Keymap for `devdocs-mode' commands."
  :name "Dev doc"
  :prefix 'cat-dev-doc-prefix
  "d" #'devdocs-delete
  "i" #'devdocs-install
  "l" #'devdocs-lookup
  "p" #'devdocs-peruse
  "s" #'devdocs-search
  "u" #'devdocs-update-all)
