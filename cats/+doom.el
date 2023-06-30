;; -*- lexical-binding: t; -*-

(use-package doom-themes
  :config
  (doom-themes-neotree-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  (defun doom-dark-theme ()
      (load-theme 'doom-nord t))
  (defun doom-light-theme ()
      (load-theme 'doom-nord-light t)))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))
