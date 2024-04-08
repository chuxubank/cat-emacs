;; -*- lexical-binding: t; -*-

(defun doom-dark-theme ()
  (load-theme 'kaolin-dark t))
(defun doom-light-theme ()
  (load-theme 'kaolin-light t))

(use-package doom-themes
  :demand t
  :hook (org-load . doom-themes-org-config))

(use-package kaolin-themes)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-minor-modes t)
  (doom-modeline-enable-word-count t))

(defun cat-enable-doom-modeline-minor-modes ()
  (setq-local doom-modeline-minor-modes t))

(defun cat-toggle-minor-modes ()
  (interactive)
  (setq doom-modeline-minor-modes (not doom-modeline-minor-modes)))
