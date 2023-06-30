;; -*- lexical-binding: t; -*-

(use-package beacon
  :hook after-init-hook)

(use-package goggles
  :hook (prog-mode-hook text-mode-hook)
  :config
  (setq-default goggles-pulse t))

(use-package org-modern
  :disabled
  :after org
  :custom
  (org-modern-table nil)
  :config
  (global-org-modern-mode))
