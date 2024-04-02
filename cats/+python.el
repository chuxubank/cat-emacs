;; -*- lexical-binding: t; -*-

(use-package python
  :ensure-system-package
  (pylint . pylint)
  (black . black)
  (pylsp . python-lsp-server))

