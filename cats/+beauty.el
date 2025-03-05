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
  :hook
  (after-init . global-diff-hl-mode)
  (after-init . diff-hl-margin-mode)
  (dired-mode . diff-hl-dired-mode)
  ((prog-mode text-mode) . diff-hl-flydiff-mode)
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package indent-bars
  :pin jcs-elpa
  :hook (prog-mode . indent-bars-mode)
  :custom
  (indent-bars-treesit-support t)
  :config
  (when (or IS-MACPLUS
            IS-WINDOWS)
    (setq indent-bars-prefer-character t)))

(use-package solaire-mode
  :hook (after-init . solaire-global-mode))

(use-package rainbow-mode
  :delight " "
  :hook (prog-mode text-mode))

(use-package valign
  :delight " 󱥣"
  :commands (valign--space valign--put-overlay))


(use-package ultra-scroll
  :vc (ultra-scroll
       :url "https://github.com/jdtsmith/ultra-scroll")
  :hook (after-init . ultra-scroll-mode)
  :custom
  (scroll-conservatively 101)
  (scroll-margin 0))

(use-package breadcrumb
  :hook (after-init . breadcrumb-mode)
  :custom
  (breadcrumb-imenu-max-length 0.9)
  :config
  (fset 'breadcrumb--project-crumbs-1 #'ignore))
