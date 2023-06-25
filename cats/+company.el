;; -*- lexical-binding: t; -*-

(use-package company
  :defer t)

(use-package company-box
  :hook company-mode)

(add-hook 'after-init-hook #'global-company-mode)
