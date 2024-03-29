;; -*- lexical-binding: t; -*-

(use-package beacon
  :hook (after-init . beacon-mode)
  :custom
  (beacon-lighter nil))

(use-package pulsar
  :hook (after-init . pulsar-global-mode))

(use-package hl-line
  :hook ((prog-mode text-mode) . hl-line-mode))

(use-package goggles
  :delight
  :hook ((prog-mode text-mode) . goggles-mode)
  :init
  (setq goggles-mode nil)
  :config
  (setq-default goggles-pulse t))

(use-package pangu-spacing
  :delight
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
  (add-hook 'nxml-mode-hook #'+pangu-spacing-disable))

(use-package diff-hl
  :delight (diff-hl-amend-mode " ")
  :hook (after-init . global-diff-hl-mode)
  :hook ((prog-mode text-mode) . diff-hl-flydiff-mode)
  :hook (dired-mode . diff-hl-dired-mode)
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
