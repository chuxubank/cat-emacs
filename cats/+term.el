;; -*- lexical-binding: t; -*-

(use-package vterm
  :bind
  (:map vterm-mode-map
        ("C-q" . #'vterm-send-next-key)))

(use-package meow-vterm
  :when (featurep 'meow)
  :vc (:url "https://github.com/accelbread/meow-vterm" :rev :newest)
  :config
  (meow-vterm-enable))
