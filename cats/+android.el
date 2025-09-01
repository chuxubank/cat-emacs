;; -*- lexical-binding: t; -*-

(use-package android-mode
  :ensure nil
  :delight " 󰀲"
  :commands #'android-root
  :init
  (defun +android-mode ()
    (when (android-root) (android-mode t)))
  :hook ((find-file dired-mode) . +android-mode)
  :custom
  (android-mode-key-prefix "\C-c p a"))

(use-package elogcat
  :commands #'elogcat
  :init
  (defvar +elogcat-logcat-package nil)
  :bind
  (:map elogcat-mode-map
        ("n" . #'next-line)
        ("p" . #'previous-line)
        ("P" . #'+elogcat-toggle-package)
        ("S" . #'+elogcat-save-buffer))
  :config
  (defun elogcat-make-status (&optional status)
    "Get a log buffer STATUS for use in the mode line."
    (format " elogcat[%s](%s)"
            (mapconcat (lambda (args) (elogcat-get-log-buffer-status args))
                       '("main" "system" "radio" "events" "crash" "kernel") "")
            +elogcat-logcat-package))
  (add-hook 'elogcat-mode-hook #'meow-motion-mode)
  (add-hook 'elogcat-mode-hook #'cat-enable-doom-modeline-minor-modes))

(defun +elogcat-toggle-package (pkg)
  "Toggle package filter."
  (interactive
   (list (completing-read "Select package: "
                          (mapcar (lambda (name)
                                    (replace-regexp-in-string "package\\:" "" name))
                                  (split-string (string-trim (shell-command-to-string "adb shell pm list package -3")) "\n")))))
  (let ((pid (string-trim (shell-command-to-string (concat "adb shell pidof " pkg))))
        (option " --pid="))
    (if (string-empty-p pid) (error "App not running."))
    (if (string-match (format "\\(%s\\)\\([0-9]*\\)" option) elogcat-logcat-command)
        (setq elogcat-logcat-command
              (if (string= (match-string 2 elogcat-logcat-command) pid)
                  (replace-match "" nil nil elogcat-logcat-command)
                (replace-match pid nil nil elogcat-logcat-command 2)))
      (setq elogcat-logcat-command  (concat elogcat-logcat-command option pid)))
    (setq +elogcat-logcat-package (format "%s:%s" pkg pid)))
  (let ((buffer-read-only nil))
    (erase-buffer))
  (elogcat-stop)
  (elogcat))

(defun +elogcat-save-buffer ()
  "Save current elogcat buffer."
  (interactive)
  (save-buffer)
  (elogcat-stop))
