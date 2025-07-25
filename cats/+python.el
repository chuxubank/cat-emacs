;; -*- lexical-binding: t; -*-

(use-package python
  :ensure-system-package
  (pylint . pylint)
  (black . black)
  (pylsp . python-lsp-server)
  (pyright . "bun add -g pyright")
  :custom
  (python-indent-guess-indent-offset-verbose nil))

(use-package pet
  :delight " 󰌠"
  :ensure-system-package (dasel)
  :hook (python-base-mode . pet-mode)
  :custom
  (pet-find-file-functions '(pet-find-file-from-project-root
                             pet-locate-dominating-file))
  :mode-hydra
  (python-base-mode
   ("Plugin"
    (("v" #'pet-verify-setup "pet verify")))))

(use-package poetry
  :disabled
  :hook (python-base-mode . poetry-tracking-mode)
  :custom
  (poetry-tracking-strategy 'switch-buffer))

(use-package uv-mode
  :hook (python-mode . uv-mode-auto-activate-hook))
