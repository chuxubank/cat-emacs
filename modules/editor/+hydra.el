;; -*- lexical-binding: t; -*-

(use-package hydra-posframe
  :vc (:url "https://github.com/Ladicle/hydra-posframe")
  :hook (after-init . hydra-posframe-mode)
  :custom
  (hydra-posframe-poshandler #'posframe-poshandler-window-bottom-center)
  (hydra-posframe-font (+random-get cat-mono-sans-fonts))
  (hydra-posframe-border-width 10))
