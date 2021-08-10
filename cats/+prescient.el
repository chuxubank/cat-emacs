(when (featurep 'company)
  (use-package company-prescient)
  (company-prescient-mode))

(when (featurep 'selectrum)
  (use-package selectrum-prescient)
  (selectrum-prescient-mode))
