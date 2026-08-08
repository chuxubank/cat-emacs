;; -*- lexical-binding: t; -*-

(unless (or EMACS29+ (package-installed-p 'use-package))
  (package-refresh-contents)
  (cat-package-install 'use-package))

(require 'use-package)

(unless (or EMACS30+ (package-installed-p 'vc-use-package))
  (cat-package-vc-install "https://github.com/slotThe/vc-use-package"))

(use-package use-package-ignore-builtin
  :vc (:url "https://github.com/cat-emacs/use-package-ignore-builtin")
  :demand t)

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
(use-package nerd-icons)
(use-package delight)

(use-package transient
  :ignore-builtin
  :pin gnu)

(use-package mode-transient
  :vc (:url "https://github.com/cat-emacs/mode-transient")
  :demand t
  :init
  (defun cat/mode-transient-title (mode kind)
    "Return a mode Transient title for MODE of KIND."
    (+with-mode-icon mode (mode-transient-default-title mode kind)))
  :custom
  (mode-transient-title-function #'cat/mode-transient-title)
  :config
  (require 'mode-transient-use-package)
  (transient-bind-q-to-quit))
