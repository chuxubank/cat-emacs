;; -*- lexical-binding: t; -*-

(use-package format-all
  :defer t)

(dolist (h '(c-mode-hook
	     lisp-mode-hook
	     lisp-interaction-mode-hook
	     emacs-lisp-mode-hook
	     js-mode-hook
	     bibtex-mode-hook))
  (add-hook h 'format-all-mode))
(add-hook 'format-all-mode-hook #'format-all-ensure-formatter)
