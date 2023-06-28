;; -*- lexical-binding: t; -*-

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-themes-treemacs-theme "doom-atom")
  (doom-themes-neotree-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  (defun doom-dark-theme ()
      (load-theme 'doom-nord t))
  (defun doom-light-theme ()
      (load-theme 'doom-nord-light t)))

(use-package doom-modeline
  :hook after-init)
