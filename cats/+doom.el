;; -*- lexical-binding: t; -*-

(defun doom-dark-theme ()
  (load-theme 'kaolin-dark t))
(defun doom-light-theme ()
  (load-theme 'kaolin-light t))

(use-package doom-themes
  :config
  (doom-themes-org-config))

(use-package kaolin-themes)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-minor-modes t))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package treemacs-nerd-icons
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))
