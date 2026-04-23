;; -*- lexical-binding: t; -*-

(pretty-hydra-define cat-android
  (:color teal :title (+with-icon "nf-md-android" "Android"))
  ("" ()))

(use-package android-mode
  :ensure nil
  :delight " 󰀲"
  :commands #'android-root
  :init
  (defun +android-mode ()
    (when (android-root) (android-mode t)))
  :hook ((find-file dired-mode) . +android-mode)
  :pretty-hydra
  (cat-android
   ("Start"
    (("a" #'android-start-app "start app")
     ("r" #'android-run "run")
     ("e" #'android-start-emulator "emulator"))
    "Build"
    (("c" #'android-gradle-build "build")
     ("C" #'android-gradle-clean "clean")
     ("t" #'android-gradle-test "test")
     ("i" #'android-gradle-install "install")
     ("u" #'android-gradle-uninstall "uninstall")
     ("f" #'android-print-flavor "flavors")
     ("R" #'android-refresh-flavors "refresh")))))

(use-package elogcat
  :commands #'elogcat
  :init
  (defvar +elogcat-logcat-package nil)
  (defvar +elogcat-min-level "V"
    "Minimum log level to display. One of V D I W E F.")
  :bind
  (:map elogcat-mode-map
        ("n" . #'next-line)
        ("p" . #'previous-line)
         ("P" . #'+elogcat-toggle-package)
         ("S" . #'+elogcat-save-buffer)
         ("L" . #'+elogcat-set-level))
  :pretty-hydra
  (cat-android
   ("Log"
    (("l" #'elogcat "elogcat"))))
  :config
  (defconst +elogcat-level-priority '("V" "D" "I" "W" "E" "F")
    "Log levels in ascending priority order.")

  (defun +elogcat-level-visible-p (level)
    "Return non-nil if LEVEL meets the minimum level threshold."
    (>= (or (cl-position level +elogcat-level-priority :test #'string=) 0)
        (or (cl-position +elogcat-min-level +elogcat-level-priority :test #'string=) 0)))

  (defun +elogcat-set-level (level)
    "Set minimum log level filter."
    (interactive
     (list (completing-read
            (format "Min level (current: %s): " +elogcat-min-level)
            '("V - Verbose" "D - Debug" "I - Info" "W - Warning" "E - Error" "F - Fatal")
            nil t)))
    (setq +elogcat-min-level (substring level 0 1))
    (message "elogcat: min level set to %s" +elogcat-min-level))

  (defun elogcat-make-status (&optional status)
    "Get a log buffer STATUS for use in the mode line."
    (format " elogcat[%s](%s)<%s>"
            (mapconcat (lambda (args) (elogcat-get-log-buffer-status args))
                       '("main" "system" "radio" "events" "crash" "kernel") "")
            +elogcat-logcat-package
            +elogcat-min-level))

  ;; Optimized process filter: strip \r cheaply, batch insert, inhibit
  ;; GC and redisplay.  Only auto-scroll windows that were already at
  ;; the tail; manual scroll/page-up stops following.
  (defun elogcat-process-filter (process output)
    "Adb PROCESS make line from OUTPUT buffer."
    (when (get-buffer elogcat-buffer)
      (with-current-buffer elogcat-buffer
        (let* ((old-max (point-max))
               (following-wins
                (cl-loop for win in (get-buffer-window-list elogcat-buffer nil t)
                         when (>= (window-point win) old-max)
                         collect win))
               (gc-cons-threshold most-positive-fixnum)
               (inhibit-redisplay t)
               (buffer-read-only nil)
               (raw (concat elogcat-pending-output output))
               ;; Strip \r without regexp — much cheaper on large chunks.
               (output (if (string-search "\r" raw)
                           (string-replace "\r" "" raw)
                         raw))
               (include elogcat-include-filter-regexp)
               (exclude elogcat-exclude-filter-regexp)
               (min-level-idx (or (cl-position +elogcat-min-level
                                               +elogcat-level-priority
                                               :test #'string=) 0))
               (pos 0)
               (chunks nil))
          (while (string-match "\n" output pos)
            (let* ((end (match-beginning 0))
                   (line (substring output pos end)))
              (setq pos (match-end 0))
              ;; Inline filter checks — skip s-match overhead when no
              ;; filter is set.
              (when (and (or (null include)
                             (string-match-p include line))
                        (or (null exclude)
                            (not (string-match-p exclude line))))
                (let ((formatted
                       (if (string-match
                            "^[0-9][0-9]-[0-9][0-9] +[0-9:.]+  *[0-9]+  *[0-9]+ \\([VDIWEF]\\) "
                            line)
                           (let* ((lvl (match-string 1 line))
                                  (face (cdr (or (assoc lvl elogcat-face-alist)
                                                 (assoc "I" elogcat-face-alist)))))
                             (when (>= (or (cl-position lvl +elogcat-level-priority
                                                        :test #'string=) 0)
                                       min-level-idx)
                               (propertize (concat line "\n") 'face face)))
                         (concat line "\n"))))
                  (when formatted
                    (push formatted chunks))))))
          (setq elogcat-pending-output (substring output pos))
          (when chunks
            (save-excursion
              (goto-char (point-max))
              (insert (apply #'concat (nreverse chunks)))))
          (let ((new-max (point-max)))
            (dolist (win following-wins)
              (set-window-point win new-max)))))))

  ;; Override: default "-T 0" (new logs only), C-u N for last N lines,
  ;; C-u for full history.  Disable font-lock (process filter handles faces).
  (defun elogcat (&optional arg)
    "Start the adb logcat process.
Without prefix, only stream new logs (-T 0).
With numeric prefix N, replay last N lines then stream.
With bare \\[universal-argument], replay full history."
    (interactive "P")
    (unless (get-process "elogcat")
      (let* ((tail-arg (cond
                        ((null arg) " -T 1")
                        ((consp arg) "")          ; C-u → full history
                        (t (format " -T %d" (prefix-numeric-value arg)))))
             (cmd (concat elogcat-logcat-command
                          (unless (s-contains? "-b" elogcat-logcat-command)
                            " -s")
                          tail-arg))
             (proc (start-process-shell-command
                    "elogcat" elogcat-buffer
                    (concat "adb shell " (shell-quote-argument cmd)))))
        (set-process-filter proc #'elogcat-process-filter)
        (set-process-sentinel proc #'elogcat-process-sentinel)
        (with-current-buffer elogcat-buffer
          (elogcat-mode t)
          (setq buffer-read-only t)
          (font-lock-mode -1))
        (switch-to-buffer elogcat-buffer)
        (goto-char (point-max)))))

  ;; Jump to the end immediately when the buffer is opened.
  (defun +elogcat-goto-end ()
    "Move point and window to the end of the elogcat buffer."
    (goto-char (point-max))
    (dolist (win (get-buffer-window-list (current-buffer) nil t))
      (set-window-point win (point-max))))

  (add-hook 'elogcat-mode-hook #'+elogcat-goto-end)
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
