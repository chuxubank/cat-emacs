;; -*- lexical-binding: t; -*-

(use-package apheleia
  :hook (after-init . apheleia-global-mode)
  :custom
  (apheleia-remote-algorithm 'local))
