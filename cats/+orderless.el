;; -*- lexical-binding: t; -*-

(use-package orderless)

(with-eval-after-load 'selectrum
  (setq selectrum-refine-candidates-function #'orderless-filter
        selectrum-highlight-candidates-function #'orderless-highlight-matches))

(with-eval-after-load 'vertico
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic-remote partial-completion)))))
