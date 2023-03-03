;; -*- lexical-binding: t; -*-

(use-package benchmark-init
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(defun cat-init-time ()
  "Prints the init time."
  (interactive)
  (message "%s init within %s" cat-emacs-name (emacs-init-time)))

(defun display-startup-echo-area-message ()
  (cat-init-time))
