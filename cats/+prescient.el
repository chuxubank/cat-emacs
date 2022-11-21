;; -*- lexical-binding: t; -*-

(use-package company-prescient
  :if (featurep 'company)
  :config
  (company-prescient-mode 1))

(use-package selectrum-prescient
  :if (featurep 'selectrum)
  :config
  (selectrum-prescient-mode 1))

(use-package vertico-prescient
  :if (featurep 'vertico)
  :config
  (vertico-prescient-mode 1))

(setq prescient-save-file (expand-file-name "prescient-save.el" cat-cache-dir))
(prescient-persist-mode 1)
