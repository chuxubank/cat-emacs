;; -*- lexical-binding: t; -*-

(use-package windmove
  :custom
  (windmove-wrap-around t)
  :config
  (windmove-default-keybindings 'super))

(use-package ace-window
  :hook (after-init . ace-window-display-mode)
  :bind
  ([remap other-window] . ace-window)
  :custom
  (aw-background nil))

(use-package transpose-frame
  :defer t)
