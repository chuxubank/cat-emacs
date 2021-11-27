;; -*- lexical-binding: t; -*-

(use-package rg
  :commands #'rg-project)

(define-key global-map (kbd "C-c p s") #'rg-project)
