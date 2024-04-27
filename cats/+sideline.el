;; -*- lexical-binding: t; -*-

(use-package sideline
  :delight
  :vc (sideline
       :url "https://github.com/chuxubank/sideline"
       :rev :newest)
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

(use-package sideline-blame
  :demand t
  :after sideline
  :custom
  (sideline-blame-commit-format " %s")
  :config
  (add-to-list 'sideline-backends-left #'sideline-blame))

(use-package sideline-eglot
  :pin jcs-elpa
  :demand t
  :after sideline eglot
  :custom
  (sideline-eglot-code-actions-prefix " ")
  :config
  (add-to-list 'sideline-backends-right #'sideline-eglot))
