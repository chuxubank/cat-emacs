;; -*- lexical-binding: t; -*-

(use-package company
  :hook (after-init . global-company-mode)
  :custom
  (company-minimum-prefix-length 0))

(use-package company-box
  :when (or (daemonp)
            (display-graphic-p))
  :hook (company-mode . company-box-mode))
