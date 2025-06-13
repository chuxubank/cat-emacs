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

(defun cat-preload-org-agenda ()
  "Preload Org agenda files, useful when running as a daemon."
  ;; Ensure org-agenda is available. org-mode should have been loaded already
  ;; via cats/+org.el, which makes org-agenda's autoloads available.
  (require 'org)
  (if (bound-and-true-p org-agenda-files)
      (progn
        (cat-benchmark 'beg "pre-loading Org agenda files.")
        (let ((files (org-agenda-files nil 'ifmode)))
          (org-agenda-prepare-buffers files)
          (cat-benchmark 'end (format "Org agenda files pre-loaded using %s files." (length files)))))
    (message "Org agenda files not set, skipping preload.")))

(when (daemonp)
  (add-hook 'emacs-startup-hook #'cat-preload-org-agenda))
