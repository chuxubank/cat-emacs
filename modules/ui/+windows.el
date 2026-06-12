;; -*- lexical-binding: t; -*-

(use-package winner
  :hook (after-init . winner-mode))

(use-package windmove
  :hook (after-init . windmove-mode)
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

(use-package golden-ratio
  :disabled
  :demand
  :init
  ;; https://github.com/roman/golden-ratio.el/issues/57#issuecomment-131472709
  (defvar golden-ratio-selected-window
    (frame-selected-window)
    "Selected window.")

  (defun golden-ratio-set-selected-window
      (&optional window)
    "Set selected window to WINDOW."
    (setq-default
     golden-ratio-selected-window (or window (frame-selected-window))))

  (defun golden-ratio-selected-window-p
      (&optional window)
    "Return t if WINDOW is selected window."
    (eq (or window (selected-window))
        (default-value 'golden-ratio-selected-window)))

  (defun golden-ratio-maybe
      (&optional arg)
    "Run `golden-ratio' if `golden-ratio-selected-window-p' returns nil."
    (interactive "p")
    (unless (golden-ratio-selected-window-p)
      (golden-ratio-set-selected-window)
      (golden-ratio arg)))
  :hook
  (after-init . golden-ratio-mode)
  :custom
  (golden-ratio-auto-scale t)
  :config
  (define-minor-mode golden-ratio-mode
    "Enable automatic window resizing with golden ratio."
    :lighter " 󰨤"
    :global t
    (if golden-ratio-mode
        (add-hook 'buffer-list-update-hook 'golden-ratio-maybe)
      (remove-hook 'buffer-list-update-hook 'golden-ratio-maybe)))
  (with-eval-after-load 'which-key
    (add-to-list 'golden-ratio-inhibit-functions #'which-key--popup-showing-p)))
