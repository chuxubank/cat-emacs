;; -*- lexical-binding: t; -*-

(use-package yaml-ts-mode
  :mode "\\.ya?ml\\'")

(use-package yaml-pro
  :hook (yaml-ts-mode . yaml-pro-ts-mode))

(use-package poly-yaml-jinja2
  :ensure nil
  :mode ("\\.poet" . poly-yaml-jinja2-mode))
