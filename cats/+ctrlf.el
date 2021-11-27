;; -*- lexical-binding: t; -*-

(use-package ctrlf)

(ctrlf-mode 1)

(add-hook 'pdf-isearch-minor-mode-hook (lambda () (ctrlf-local-mode -1)))
