;; -*- lexical-binding: t; -*-

(use-package vterm
  :bind
  (:map vterm-mode-map
        ("C-q" . #'vterm-send-next-key)))

(when (package-installed-p 'meow)
  (use-package meow-vterm
    :vc (meow-vterm
         :url "https://github.com/accelbread/meow-vterm"
         :rev :newest)
    :demand t
    :after vterm meow
    :config
    (meow-vterm-enable)))
