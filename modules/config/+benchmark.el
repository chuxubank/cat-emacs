;; -*- lexical-binding: t; -*-

(use-package benchmark-init
  :demand
  :hook (after-init . benchmark-init/deactivate))

(defun cat-init-time ()
  "Prints the init time."
  (interactive)
  (message "%s init within %s" cat-emacs-name (emacs-init-time)))

(defun display-startup-echo-area-message ()
  (cat-init-time))
