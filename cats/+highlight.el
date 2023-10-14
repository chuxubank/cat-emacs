;; -*- lexical-binding: t; -*-

(use-package beacon
  :when (or (daemonp)
            (display-graphic-p))
  :hook (after-init . beacon-mode)
  :custom
  (beacon-lighter nil))

(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :init
  (setq goggles-mode nil)
  :config
  (setq-default goggles-pulse t)
  (+change-lighter 'goggles-mode nil))

(use-package pangu-spacing
  :hook (text-mode . pangu-spacing-mode)
  :custom
  (pangu-spacing-real-insert-separtor t)
  :init
  (setq pangu-spacing-mode nil)
  :config
  ;; Add `note-property' and `table-row'
  (defun pangu-spacing-org-mode-at-special-region ()
    (interactive)
    (let ((element (org-element-at-point)))
      (when (or (member (org-element-type element)
                        '(src-block
                          keyword
                          example-block
                          export-block
                          latex-environment
                          planning
                          node-property
                          table-row))
                (member (car (org-element-context element))
                        '(inline-src-block
                          timestamp
                          link
                          code
                          verbatim)))
        t)))
  (defun +pangu-spacing-disable ()
    (pangu-spacing-mode -1))
  (add-hook 'nxml-mode-hook #'+pangu-spacing-disable)
  (+change-lighter 'pangu-spacing-mode nil))

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
            (:eval (if pangu-spacing-mode "P" ""))
            ")"))

(highlight-mode)
