;; -*- lexical-binding: t; -*-

(use-package company-prescient
  :disabled
  :when (package-installed-p 'company)
  :hook (company-mode . company-prescient-mode))

(use-package selectrum-prescient
  :disabled
  :when (package-installed-p 'selectrum)
  :hook (selectrum-mode . selectrum-prescient-mode))

(use-package vertico-prescient
  :when (package-installed-p 'vertico)
  :hook (vertico-mode . vertico-prescient-mode))

(use-package corfu-prescient
  :when (package-installed-p 'corfu)
  :hook (corfu-mode . corfu-prescient-mode))

(setq prescient-save-file (expand-file-name "prescient-save.el" cat-cache-dir))
(with-eval-after-load 'prescient
  (prescient-persist-mode 1))
