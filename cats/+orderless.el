;; -*- lexical-binding: t; -*-

(use-package orderless)

(when (featurep 'selectrum)
  (setq selectrum-refine-candidates-function #'orderless-filter
        selectrum-highlight-candidates-function #'orderless-highlight-matches))

(when (featurep 'vertico)
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic-remote partial-completion)))))
