;; -*- lexical-binding: t; -*-

(use-package beancount
  :ensure-system-package
  (beancount-language-server . beancount-language-server)
  :pin jcs-elpa
  :init
  (setq beancount-mode-map-prefix [(control c) (control c)])
  (add-hook 'beancount-mode-hook #'outline-minor-mode)
  (add-hook 'beancount-mode-hook #'flymake-bean-check-enable)
  (add-hook 'beancount-mode-hook #'cat-disable-electric-indent-chars)
  :custom
  (beancount-use-ido nil)
  :mode-hydra
  (beancount-mode
   ("Tools"
    (("f" #'beancount-fava "fava"))))
  :config
  (define-key beancount-mode-map (kbd "C-c C-n") #'outline-next-visible-heading)
  (define-key beancount-mode-map (kbd "C-c C-p") #'outline-previous-visible-heading)

  (defun beancount--fava-filter (_process output)
    "Open fava url as soon as the address is announced."
    (with-current-buffer "*fava*" (insert output))
    (if-let ((url (string-match "Starting Fava on \\(http://.+:[0-9]+\\)" output)))
        (browse-url (match-string 1 output)))))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(beancount-mode . ("beancount-language-server" "--stdio"
                                   :initializationOptions
                                   (:journal_file "ledger/ledger.beancount")))))
