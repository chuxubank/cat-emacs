(use-package company
  :hook (after-init . global-company-mode))

(add-hook 'shell-mode-hook (lambda () (company-mode -1)))
