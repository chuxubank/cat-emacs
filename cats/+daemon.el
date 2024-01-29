;; -*- lexical-binding: t; -*-

(defun cat-client-frame-config ()
  (cat-benchmark 'beg "configuring new frame.")
  (cat-load-theme)
  (if (display-graphic-p)
      (progn
        (message "In GUI.")
        (select-frame-set-input-focus (selected-frame))
        (add-hook 'eldoc-mode-hook #'eldoc-box-hover-at-point-mode)
        (unless eldoc-box-hover-at-point-mode
          (eldoc-box-hover-at-point-mode 1))
        (cat! "+autodark")
        (cat! "+font")
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

(defun cat-daemon-preload ()
  (cat-benchmark 'beg "daemon preload.")
  (require 'org)
  (cat-benchmark 'end "daemon preload."))

(when (daemonp)
  (add-hook 'after-init-hook #'cat-daemon-preload))
