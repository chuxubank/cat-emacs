;; -*- lexical-binding: t; -*-

(use-package sideline
  :delight
  :hook (after-init . global-sideline-mode)
  :custom
  (sideline-display-backend-name t))

(use-package sideline-flycheck
  :hook (flycheck-mode . sideline-flycheck-setup)
  :config
  (add-to-list 'sideline-backends-right #'sideline-flycheck))

(use-package sideline-flymake
  :demand t
  :after flymake
  :config
  (add-to-list 'sideline-backends-right #'sideline-flymake))
