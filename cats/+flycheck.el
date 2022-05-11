;; -*- lexical-binding: t; -*-

(use-package flycheck
  :defer t
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit))

(use-package flycheck-pos-tip
  :after flycheck
  :hook (flycheck-mode . flycheck-pos-tip-mode))

(use-package flycheck-posframe
  :disabled
  :after flycheck
  :hook (flycheck-mode . flycheck-posframe-mode))
