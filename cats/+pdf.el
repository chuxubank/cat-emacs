(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :init
  (setq pdf-view-use-scaling t)
  :config
  (pdf-tools-install))

(add-hook 'pdf-view-mode-hook (lambda () (pdf-view-themed-minor-mode 1)))

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))
