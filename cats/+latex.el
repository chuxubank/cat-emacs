;; -*- lexical-binding: t; -*-

(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-master nil)
  (TeX-engine 'xetex))

(use-package reftex
  :ensure nil
  :custom
  (reftex-default-bibliography cat-default-bibliography-files))
