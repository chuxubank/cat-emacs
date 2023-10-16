;; -*- lexical-binding: t; -*-

(use-package undo-fu-session
  :custom
  (undo-fu-session-directory (concat cat-etc-dir "undo-fu-session"))
  :hook (after-init . undo-fu-session-global-mode))

(use-package vundo
  :bind
  (:map ctl-x-map
        ("u" . vundo))
  :custom
  (vundo-glyph-alist vundo-unicode-symbols))
