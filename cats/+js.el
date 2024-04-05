;; -*- lexical-binding: t; -*-

(use-package js
  :custom
  (js-indent-level 2))

(use-package json-mode
  :ensure-system-package (prettier)
  :bind
  (:map json-mode-map
        ("C-c C-w" . json-mode-kill-path)))
