;; -*- lexical-binding: t; -*-

(use-package python
  :ensure-system-package
  (pylint . pylint)
  (black . black)
  (pylsp . python-lsp-server)
  (pyright . "pnpm add -g pyright")
  :custom
  (python-indent-guess-indent-offset-verbose nil)
  :mode-hydra
  (python-base-mode
   ("LSP"
    (("e" eglot-hydra/body "eglot"))
    "Plugin"
    (("p" #'poetry "poetry")
     ("v" #'pet-verify-setup "pet verify")))))

(use-package pet
  :delight " 󰌠"
  :ensure-system-package (dasel)
  :hook (python-base-mode . pet-mode)
  :custom
  (pet-find-file-functions '(pet-find-file-from-project-root
                             pet-locate-dominating-file)))

(use-package poetry
  :hook (python-base-mode . poetry-tracking-mode)
  :custom
  (poetry-tracking-strategy 'switch-buffer))
