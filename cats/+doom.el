;; -*- lexical-binding: t; -*-

(defun doom-dark-theme ()
  (load-theme 'kaolin-dark t))
(defun doom-light-theme ()
  (load-theme 'kaolin-light t))

(use-package doom-themes
  :config
  (doom-themes-neotree-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package kaolin-themes
  :config
  (kaolin-treemacs-theme))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))
