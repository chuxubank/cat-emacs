;; -*- lexical-binding: t; -*-

(use-package orderless)

(with-eval-after-load 'vertico
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic-remote partial-completion)))))
