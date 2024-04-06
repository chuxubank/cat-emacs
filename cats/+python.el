;; -*- lexical-binding: t; -*-

(use-package python
  :ensure-system-package
  (pylint . pylint)
  (black . black)
  (pylsp . python-lsp-server)
  :custom
  (python-indent-guess-indent-offset-verbose nil)
  :mode-hydra
  (python-base-mode
   ("LSP"
    (("e" eglot-hydra/body "eglot"))
    "Plugin"
    (("p" poetry "poetry")))))

(use-package pet
  :delight " 󰌠"
  :ensure-system-package (dasel)
  :hook (python-base-mode . pet-mode))

(use-package poetry
  :hook (python-base-mode . poetry-tracking-mode)
  :custom
  (poetry-tracking-strategy 'switch-buffer))
