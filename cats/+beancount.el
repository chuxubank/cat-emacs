;; -*- lexical-binding: t; -*-

(use-package beancount
  :straight (beancount :host github :repo "beancount/beancount-mode")
  :init
  (setq beancount-mode-map-prefix [(control c) (control c)])
  (add-hook 'beancount-mode-hook #'outline-minor-mode)
  :mode ("\\.beancount\\'" . beancount-mode)
  :config
  (define-key beancount-mode-map (kbd "C-c C-n") #'outline-next-visible-heading)
  (define-key beancount-mode-map (kbd "C-c C-p") #'outline-previous-visible-heading))
