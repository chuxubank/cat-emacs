(use-package selectrum)
(selectrum-mode)

(when (featurep 'orderless)
  (setq selectrum-refine-candidates-function #'orderless-filter)
  (setq selectrum-highlight-candidates-function #'orderless-highlight-matches))

(savehist-mode)
