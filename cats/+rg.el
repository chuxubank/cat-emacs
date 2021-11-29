;; -*- lexical-binding: t; -*-

(use-package rg
  :defer t)

(define-key global-map (kbd "C-c s") #'rg-menu)
