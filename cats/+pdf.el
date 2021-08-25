(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :init
  (setq pdf-view-use-scaling t)
  :config
  (pdf-tools-install)
  (defun nano-modeline-pdf-view-mode ()
    (let ((buffer-name (format-mode-line "%b"))
	  (mode-name   (nano-mode-name))
	  (branch      (vc-branch))
	  (page-number (concat
			(number-to-string (pdf-view-current-page)) "/"
			(or (ignore-errors
			      (number-to-string (pdf-cache-number-of-pages)))
			    "???"))))
      (nano-modeline-compose
       "RO"
       buffer-name
       (concat "("
	       mode-name
	       (if branch (concat ", " (propertize branch 'face 'italic)))
	       ")" )
       page-number))))

(add-hook 'pdf-view-mode-hook (lambda () (pdf-view-themed-minor-mode 1)))

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))
