;; -*- lexical-binding: t; -*-

(use-package vterm
  :bind
  (:map vterm-mode-map
        ("C-q" . #'vterm-send-next-key)))

(when (package-installed-p 'meow)
  (use-package meow-vterm
    :after meow vterm
    :vc (:url "https://github.com/accelbread/meow-vterm" :rev :newest)
    :config
    (meow-vterm-enable)))
