;; -*- lexical-binding: t; -*-

(use-package company
  :hook (after-init-hook . global-company-mode))

(use-package company-box
  :hook (company-mode . company-box-mode))
