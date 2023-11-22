;; -*- lexical-binding: t; -*-

(use-package winner
  :hook (after-init . winner-mode))

(use-package windmove
  :custom
  (windmove-wrap-around t)
  :config
  (windmove-default-keybindings 'super))

(use-package winum
  :hook (after-init . winum-mode)
  :init
  (defvar-keymap winum-keymap
    "M-1" #'winum-select-window-1
    "M-2" #'winum-select-window-2
    "M-3" #'winum-select-window-3
    "M-4" #'winum-select-window-4
    "M-5" #'winum-select-window-5
    "M-6" #'winum-select-window-6
    "M-7" #'winum-select-window-7
    "M-8" #'winum-select-window-8
    "M-9" #'winum-select-window-9)
  :custom
  (winum-auto-setup-mode-line nil))

(use-package ace-window
  :custom
  (aw-background nil)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package transpose-frame)
