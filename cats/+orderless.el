(use-package orderless)
(when (featurep 'selectrum)
  (setq selectrum-refine-candidates-function #'orderless-filter
	selectrum-highlight-candidates-function #'orderless-highlight-matches))
