
(require 'polymode)
(require 'treesit nil t)
(require 'poly-jinja2)

(when (featurep 'treesit)
  (unless (boundp 'poly-yaml-ts-hostmode)
    (define-hostmode poly-yaml-ts-hostmode :mode 'yaml-ts-mode)))

;;;###autoload (autoload 'poly-yaml-jinja2-mode "poly-yaml-jinja2" "Polymode for Jinja2 templating in YAML." t)
(define-polymode poly-yaml-jinja2-mode nil
  "Polymode for Jinja2 templating in YAML."
  :hostmode (if (eq 'yaml-ts-mode
                    (ignore-errors
                      (with-temp-buffer
                        (set-visited-file-name
                         (concat temporary-file-directory
                                 (make-temp-name "")
                                 ".yaml")
                         t t)
                        (set-auto-mode)
                        major-mode)))
                'poly-yaml-ts-hostmode
              'poly-yaml-hostmode)
  :innermodes '(poly-jinja2-innermode))

;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.ya?ml\\.\\(j2\\|jinja2\\)" . poly-yaml-jinja2-mode))
