(use-package telega
  :commands #'telega)

(when (featurep 'selectrum)
  (setq telega-completing-read-function #'selectrum-completing-read))


