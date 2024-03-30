;; -*- lexical-binding: t; -*-

(use-package python
  :ensure-system-package
  (pylint . pylint)
  (black . black)
  (pylsp . "pip install python-lsp-server"))

