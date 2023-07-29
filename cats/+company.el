;; -*- lexical-binding: t; -*-

(use-package company
  :hook (after-init . global-company-mode))

(use-package company-box
  :when (or (daemonp)
            (display-graphic-p))
  :hook (company-mode . company-box-mode))
