;; -*- lexical-binding: t; -*-

(use-package python
  :font-role (code-python :modes python-base-mode)
  :ensure-system-package
  (pylint . pylint)
  (black . black)
  (pylsp . python-lsp-server)
  (pyright . "bun add -g pyright")
  :custom
  (python-indent-guess-indent-offset-verbose nil))

(use-package pet
  :delight (pet-mode (:eval (+with-icon "nf-md-language_python" " ")))
  :ensure-system-package (dasel)
  :hook (python-base-mode . pet-mode)
  :custom
  (pet-find-file-functions '(pet-find-file-from-project-root
                             pet-locate-dominating-file))
  :major-transient
  (python-base-mode
   ["Plugin"
    ("v" "pet verify" pet-verify-setup)]))

(use-package poetry
  :cat
  :hook (python-base-mode . poetry-tracking-mode)
  :custom
  (poetry-tracking-strategy 'switch-buffer))

(use-package uv-mode
  :hook (python-mode . uv-mode-auto-activate-hook))
