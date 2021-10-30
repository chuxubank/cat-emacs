(use-package company)

(global-company-mode 1)

(add-hook 'shell-mode-hook (lambda () (company-mode -1)))
