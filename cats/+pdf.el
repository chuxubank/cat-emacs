(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
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
