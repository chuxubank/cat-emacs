;; -*- lexical-binding: t; -*-

(use-package windmove
  :custom
  (windmove-wrap-around nil)
  (windmove-create-window t)
  :config
  (windmove-default-keybindings)
  (windmove-display-default-keybindings))

(use-package ace-window
  :hook (after-init . ace-window-display-mode)
  :bind
  ([remap other-window] . ace-window)
  :custom
  (aw-background nil))

(use-package transpose-frame
  :defer t)
