;; -*- lexical-binding: t; -*-

(unless (or EMACS29+ (package-installed-p 'use-package))
  (package-refresh-contents)
  (cat-package-install 'use-package))

(require 'use-package)
(require 'ignore-builtin)

(setq-default
 use-package-always-ensure t
 use-package-always-defer t
 use-package-vc-prefer-newest t)

(use-package no-littering
  :demand
  :init
  (setq no-littering-etc-directory cat-etc-dir
        no-littering-var-directory cat-cache-dir)
  :config
  (no-littering-theme-backups)
  (let ((dir (no-littering-expand-var-file-name "lock-files/")))
    (make-directory dir t)
    (setq lock-file-name-transforms `((".*" ,dir t)))))

(use-package system-packages)
(use-package delight)
(use-package major-mode-hydra
  :demand t
  :init
  (defun cat/major-mode-hydra-title-generator (_)
    `(+with-mode-icon major-mode (s-concat (format-mode-line mode-name) " Commands")))
  :custom
  (major-mode-hydra-invisible-quit-key "q")
  (major-mode-hydra-title-generator #'cat/major-mode-hydra-title-generator))

(unless (or EMACS30+ (package-installed-p 'vc-use-package))
  (cat-package-vc-install "https://github.com/slotThe/vc-use-package"))
