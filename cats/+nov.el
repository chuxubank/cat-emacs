;; -*- lexical-binding: t; -*-

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-save-place-file (concat cat-cache-dir "nov-places")))
