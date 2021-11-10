(use-package smartparens
  :defer t
  :config
  (require 'smartparens-config))

(dolist (h '(c-mode-hook org-mode-hook))
  (add-hook h 'smartparens-mode))

(dolist (h '(lisp-mode-hook lisp-interaction-mode-hook emacs-lisp-mode-hook))
  (add-hook h 'smartparens-strict-mode))
