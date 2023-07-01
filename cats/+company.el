;; -*- lexical-binding: t; -*-

(use-package company
  :hook (after-init . global-company-mode))

(use-package company-box
  :hook company-mode)
