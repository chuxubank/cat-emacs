;; -*- lexical-binding: t; -*-

(use-package caddyfile-mode
  :mode
  (("Caddyfile\\'" . caddyfile-mode)
   ("caddy\\.conf\\'" . caddyfile-mode)))


(defun cat-caddyfile-hook ()
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode nil))

(add-hook 'caddyfile-mode-hook #'cat-caddyfile-hook)
