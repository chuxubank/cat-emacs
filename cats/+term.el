;; -*- lexical-binding: t; -*-

(use-package vterm
  :bind
  (:map vterm-mode-map
        ("C-q" . #'vterm-send-next-key)))

(use-package meow-vterm
  :when (featurep 'meow)
  :straight (meow-vterm :host github :repo "accelbread/meow-vterm")
  :config
  (meow-vterm-enable))
