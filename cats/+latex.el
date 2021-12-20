;; -*- lexical-binding: t; -*-

(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :defer t)

(setq TeX-auto-save t
      TeX-parse-self t)

(setq-default TeX-master nil
	      TeX-engine 'xetex)
