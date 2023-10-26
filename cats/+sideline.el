;; -*- lexical-binding: t; -*-

(use-package sideline
  :custom
  (sideline-display-backend-name t)
  :hook (after-init . global-sideline-mode))

(use-package sideline-flycheck
  :hook (flycheck-mode . sideline-flycheck-setup)
  :config
  (add-to-list 'sideline-backends-right #'sideline-flycheck))

(use-package sideline-flymake
  :after flymake
  :config
  (add-to-list 'sideline-backends-right #'sideline-flymake))
