;; -*- lexical-binding: t; -*-

(when (package-installed-p 'company)
  (use-package company-prescient
    :hook (company-mode . company-prescient-mode)))

(when (package-installed-p 'selectrum)
  (use-package selectrum-prescient
    :hook (selectrum-mode . selectrum-prescient-mode)))

(when (package-installed-p 'vertico)
  (use-package vertico-prescient
    :hook (vertico-mode . vertico-prescient-mode)))

(when (package-installed-p 'corfu)
  (use-package corfu-prescient
    :hook (corfu-mode . corfu-prescient-mode)))

(with-eval-after-load 'prescient
  (prescient-persist-mode 1))
