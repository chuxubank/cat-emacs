(use-package tex
  :ensure auctex
  :defer t)

(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))
