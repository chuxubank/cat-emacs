;; -*- lexical-binding: t; -*-

(use-package which-key
  :hook (after-init . which-key-mode)
  :custom
  (which-key-use-C-h-commands nil)
  (which-key-lighter nil))

(defun cat-which-key-buffer-show-p ()
  "Detect whether `which-key' show up."
  (and which-key--buffer
       (window-live-p (get-buffer-window which-key--buffer))))
