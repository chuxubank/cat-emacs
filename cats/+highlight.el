;; -*- lexical-binding: t; -*-

(use-package beacon
  :config
  (beacon-mode 1))

(use-package goggles
  :hook (prog-mode text-mode)
  :config
  (setq-default goggles-pulse t))

(use-package org-modern
  :after org
  :custom
  (org-modern-table nil)
  :config
  (global-org-modern-mode))
