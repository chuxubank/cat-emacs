;; -*- lexical-binding: t; -*-

(use-package beacon
  :when (or (daemonp)
            (display-graphic-p))
  :hook (after-init . beacon-mode))

(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t))

(use-package org-modern
  :disabled
  :after org
  :custom
  (org-modern-table nil)
  :config
  (global-org-modern-mode))
