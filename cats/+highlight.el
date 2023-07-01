;; -*- lexical-binding: t; -*-

(use-package beacon
  :hook after-init)

(use-package goggles
  :hook (prog-mode text-mode)
  :config
  (setq-default goggles-pulse t))

(use-package org-modern
  :disabled
  :after org
  :custom
  (org-modern-table nil)
  :config
  (global-org-modern-mode))
