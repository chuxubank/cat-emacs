;; -*- lexical-binding: t; -*-

(use-package company-prescient
  :cat (modulep! company)
  :hook (company-mode . company-prescient-mode))

(use-package vertico-prescient
  :cat (modulep! vertico)
  :hook (vertico-mode . vertico-prescient-mode))

(use-package corfu-prescient
  :cat (modulep! corfu)
  :hook (corfu-mode . corfu-prescient-mode))

(with-eval-after-load 'prescient
  (prescient-persist-mode 1))
