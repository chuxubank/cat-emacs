(when (featurep 'company)
  (use-package company-prescient)
  (company-prescient-mode 1))

(when (featurep 'selectrum)
  (use-package selectrum-prescient)
  (selectrum-prescient-mode 1))

(setq prescient-save-file (expand-file-name "prescient" cat-cache-dir))
(prescient-persist-mode 1)
