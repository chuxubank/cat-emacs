(defun compose-preview--find-project-root ()
  "Find the root of the project by locating the 'gradlew' file."
  (expand-file-name (locate-dominating-file default-directory "gradlew")))

(defun compose-preview--get-init-script ()
  "Return the absolute path to the init script."
  (let ((package-source-file (locate-library "compose-preview")))
    (when package-source-file
      (expand-file-name "preview.init.gradle.kts"
                        (file-name-directory package-source-file)))))

(defun compose-preview-gallery ()
  "Show a gallery of all @Preview composables in the project."
  (interactive)
  (let* ((project-root (compose-preview--find-project-root))
         (init-script (compose-preview--get-init-script)))
    (if project-root
        (progn
          (message "Starting Compose preview gallery...")
          (let ((log-buffer (get-buffer-create "*compose-preview-log*")))
            (with-current-buffer log-buffer
              (erase-buffer)
              (let ((exit-code (call-process "sh"
                                             nil ; stdin
                                             log-buffer ; stdout/stderr
                                             nil ; display
                                             "-c"
                                             (format "cd %s && ./gradlew runPreview --init-script %s"
                                                     (shell-quote-argument project-root)
                                                     (shell-quote-argument init-script)))))
                (unless (zerop exit-code)
                  (if noninteractive
                      (princ (with-current-buffer log-buffer (buffer-string)))
                    (display-buffer log-buffer))
                  (message "Compose preview failed. See *compose-preview-log* for details."))))))
      (message "Could not find project root (no 'gradlew' file found)."))))

(provide 'compose-preview)