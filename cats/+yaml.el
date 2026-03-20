;; -*- lexical-binding: t; -*-

(use-package yaml-ts-mode
  :mode "\\.ya?ml\\'")

(use-package yaml-pro
  :hook (yaml-ts-mode . yaml-pro-ts-mode)
  :bind
  (:map yaml-pro-ts-mode-map
        ("M-<up>" . #'yaml-pro-ts-move-subtree-up)
        ("M-<down>" . #'yaml-pro-ts-move-subtree-down)
        ("M-<left>" . #'yaml-pro-ts-unindent-subtree)
        ("M-<right>" . #'yaml-pro-ts-indent-subtree)))
