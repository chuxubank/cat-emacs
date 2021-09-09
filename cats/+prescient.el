(when (featurep 'company)
  (use-package company-prescient)
  (company-prescient-mode))

(when (featurep 'selectrum)
  (use-package selectrum-prescient)
  (selectrum-prescient-mode))

(setq prescient-save-file (expand-file-name "prescient" cat-cache-dir))
(prescient-persist-mode 1)
