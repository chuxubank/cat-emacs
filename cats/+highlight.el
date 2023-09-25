;; -*- lexical-binding: t; -*-

(use-package beacon
  :when (or (daemonp)
            (display-graphic-p))
  :hook (after-init . beacon-mode)
  :custom
  (beacon-lighter nil))

(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t)
  (+change-lighter 'goggles-mode nil))

(use-package org-modern
  :disabled
  :after org
  :custom
  (org-modern-table nil)
  :config
  (global-org-modern-mode))

(define-minor-mode highlight-mode
  "Highlight mode."
  :global t
  :lighter (" ("
            (:eval (if beacon-mode "B" ""))
            (:eval (if goggles-mode "G" ""))
            ")"))

(highlight-mode)
