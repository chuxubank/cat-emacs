(use-package tex
  :ensure auctex
  :defer t)

(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))

(setq TeX-auto-save t
      TeX-parse-self t)

(setq-default TeX-master nil
	      TeX-engine 'xetex)
