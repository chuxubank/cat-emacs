;; -*- lexical-binding: t; -*-

(defcustom cat-startup-idle-preload-delay 30
  "Idle time before startup preload."
  :type 'integer
  :group 'cat-emacs)

(defvar cat-idle-preload-hook nil
  "Hook to run after `emacs-startup-hook' with idle time.")

(defun cat-client-frame-config ()
  (cat-benchmark 'beg "configuring new frame.")
  (if (display-graphic-p)
      (progn
        (message "In GUI.")
        (select-frame-set-input-focus (selected-frame))
        (add-hook 'eldoc-mode-hook #'eldoc-box-hover-at-point-mode)
        (unless eldoc-box-hover-at-point-mode
          (eldoc-box-hover-at-point-mode 1))
        (tab-bar--load-buttons))
    (message "In TUI.")
    (remove-hook 'eldoc-mode-hook #'eldoc-box-hover-at-point-mode)
    (when eldoc-box-hover-at-point-mode
      (eldoc-box-hover-at-point-mode 0)))
  (when IS-MACPORT
    (setq mac-system-move-file-to-trash-use-finder (display-graphic-p)))
  (cat-benchmark 'end "configuring new frame."))

(add-hook 'server-after-make-frame-hook #'cat-client-frame-config)

(with-eval-after-load 'dashboard
  (defun cat-daemon-init-buffer ()
    (get-buffer-create dashboard-buffer-name))
  (setq initial-buffer-choice #'cat-daemon-init-buffer))

(defun cat-run-idle-preload ()
  "The function to run the `cat-idle-preload-hook'."
  (run-hooks 'cat-idle-preload-hook)
  (cat-benchmark 'end "idle preload."))

(defun cat-idle-preload ()
  "The function to schedule the idle preload time."
  (cat-benchmark 'beg "idle preload.")
  (message "Will start preload if idle %ss" cat-startup-idle-preload-delay)
  (run-with-idle-timer
   cat-startup-idle-preload-delay
   nil
   #'cat-run-idle-preload))

(add-hook 'emacs-startup-hook #'cat-idle-preload)
