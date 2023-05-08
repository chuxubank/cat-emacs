;; -*- lexical-binding: t; -*-

(use-package company)

(use-package company-box
  :hook company-mode)

(global-company-mode 1)

(add-hook 'shell-mode-hook (lambda () (company-mode -1)))
