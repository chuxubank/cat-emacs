;; -*- lexical-binding: t; -*-

(defvar cat-gui-loaded nil
  "Whether cat gui config has been loaded.")

(defun cat-client-frame-config ()
  (message "Start config new frame.")
  (if (display-graphic-p)
      (progn
        (message "In GUI.")
        (select-frame-set-input-focus (selected-frame))
        (add-hook 'eldoc-mode-hook #'eldoc-box-hover-at-point-mode)
        (unless eldoc-box-hover-at-point-mode
          (eldoc-box-hover-at-point-mode 1))
        (unless cat-gui-loaded
          (cat! "+autodark")
          (cat! "+font")
          (setq cat-gui-loaded t))
        (when IS-MACPORT
          (setq mac-system-move-file-to-trash-use-finder t)))
    (message "In TUI.")
    (cat-load-theme)
    (remove-hook 'eldoc-mode-hook #'eldoc-box-hover-at-point-mode)
    (when eldoc-box-hover-at-point-mode
      (eldoc-box-hover-at-point-mode 0))
    (when IS-MACPORT
      (setq mac-system-move-file-to-trash-use-finder nil))))

(add-hook 'server-after-make-frame-hook #'cat-client-frame-config)

(defun cat-daemon-preload ()
  (cat-benchmark 'beg "Daemon Preload")
  (require 'org)
  (cat-benchmark 'end "Daemon Preload"))

(add-hook 'after-init-hook #'cat-daemon-preload)
