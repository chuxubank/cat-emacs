;; -*- lexical-binding: t; -*-

(use-package beancount
  :pin jcs-elpa
  :init
  (setq beancount-mode-map-prefix [(control c) (control c)])
  (add-hook 'beancount-mode-hook #'outline-minor-mode)
  (add-hook 'beancount-mode-hook #'flymake-bean-check-enable)
  (add-hook 'beancount-mode-hook #'cat-disable-electric-indent-chars)
  :custom
  (beancount-use-ido nil)
  :config
  (define-key beancount-mode-map (kbd "C-c C-n") #'outline-next-visible-heading)
  (define-key beancount-mode-map (kbd "C-c C-p") #'outline-previous-visible-heading)

  (defun beancount--fava-filter (process output)
    "Open fava url as soon as the address is announced."
    (if-let ((url-index (string-match "\\(http://.+:[0-9]+\\)" output)))
        (browse-url (match-string 1 output)))))
