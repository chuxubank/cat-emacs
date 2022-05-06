;; -*- lexical-binding: t; -*-

(use-package format-all
  :defer t)

(add-hook 'prog-mode-hook 'format-all-mode)
(add-hook 'format-all-mode-hook #'format-all-ensure-formatter)
