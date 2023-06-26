;; -*- lexical-binding: t; -*-

(use-package company-prescient
  :when (featurep 'company)
  :hook (company-mode . company-prescient-mode))

(use-package selectrum-prescient
  :when (featurep 'selectrum)
  :hook (selectrum-mode . selectrum-prescient-mode))

(use-package vertico-prescient
  :when (featurep 'vertico)
  :hook (vertico-mode . vertico-prescient-mode))

(setq prescient-save-file (expand-file-name "prescient-save.el" cat-cache-dir))
(with-eval-after-load 'prescient
  (prescient-persist-mode 1))
