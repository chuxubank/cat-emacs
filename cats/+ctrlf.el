(use-package ctrlf)
(add-hook 'after-init-hook #'ctrlf-mode)
(add-hook 'pdf-isearch-minor-mode-hook (lambda () (ctrlf-local-mode -1)))
