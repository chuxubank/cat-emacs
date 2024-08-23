;; -*- lexical-binding: t; -*-

(defvar cat-font-loaded nil
  "Whether the font is loaded.")

(defun cat-client-frame-config ()
  (cat-benchmark 'beg "configuring new frame.")
  (if (display-graphic-p)
      (progn
        (message "In GUI.")
        (select-frame-set-input-focus (selected-frame))
        (add-hook 'eldoc-mode-hook #'eldoc-box-hover-at-point-mode)
        (unless eldoc-box-hover-at-point-mode
          (eldoc-box-hover-at-point-mode 1))
        (unless cat-font-loaded
          (cat-setup-fonts)
          (setq cat-font-loaded t))
        (when IS-MACPORT
          (setq mac-system-move-file-to-trash-use-finder t)))
    (message "In TUI.")
    (remove-hook 'eldoc-mode-hook #'eldoc-box-hover-at-point-mode)
    (when eldoc-box-hover-at-point-mode
      (eldoc-box-hover-at-point-mode 0))
    (when IS-MACPORT
      (setq mac-system-move-file-to-trash-use-finder nil)))
  (cat-benchmark 'end "configuring new frame."))

(add-hook 'server-after-make-frame-hook #'cat-client-frame-config)

(with-eval-after-load 'dashboard
  (defun cat-daemon-init-buffer ()
    (get-buffer-create dashboard-buffer-name))
  (setq initial-buffer-choice #'cat-daemon-init-buffer))
