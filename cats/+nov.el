;; -*- lexical-binding: t; -*-

(use-package nov
  :straight t
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-save-place-file (concat cat-cache-dir "nov-places")))
