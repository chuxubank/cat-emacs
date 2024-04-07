;; -*- lexical-binding: t; -*-

(use-package js
  :ensure-system-package
  (node . "pnpm env use -g lts")
  :custom
  (js-indent-level 2))

(use-package json-mode
  :ensure-system-package
  (prettier . "pnpm add -g prettier")
  :bind
  (:map json-mode-map
        ("C-c C-w" . json-mode-kill-path)))
