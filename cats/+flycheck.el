;; -*- lexical-binding: t; -*-

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path 'inherit))

(use-package flycheck-pos-tip
  :disabled
  :after flycheck
  :hook (flycheck-mode . flycheck-pos-tip-mode))

(use-package flycheck-posframe
  :disabled
  :after flycheck
  :hook (flycheck-mode . flycheck-posframe-mode))
