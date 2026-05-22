;;; compose-preview.el --- Paparazzi previews for Jetpack Compose -*- lexical-binding: t; -*-

;;; Commentary:

;; Generate a temporary Paparazzi test for the current Android module, run the
;; Paparazzi Gradle task, and open the generated PNG snapshots from Emacs.

;;; Code:

(require 'cl-lib)
(require 'compile)
(require 'subr-x)
(require 'android-mode nil t)

(declare-function android--flavor-variants "android-mode" (module))
(declare-function android--select-module "android-mode" ())

(defgroup compose-preview nil
  "Preview Jetpack Compose @Preview functions with Paparazzi."
  :group 'tools
  :prefix "compose-preview-")

(defcustom compose-preview-default-variant "debug"
  "Android build variant used for Paparazzi preview tasks."
  :type 'string
  :group 'compose-preview)

(defcustom compose-preview-paparazzi-version "2.0.0-alpha02"
  "Paparazzi version injected by the Gradle init script."
  :type 'string
  :group 'compose-preview)

(defcustom compose-preview-disable-ksp2 nil
  "Whether to pass -Pksp.useKSP2=false to the preview Gradle build.
Recent KSP releases no longer support KSP1, so this is disabled by default."
  :type 'boolean
  :group 'compose-preview)

(defcustom compose-preview-use-legacy-android-dsl t
  "Whether to pass -Pandroid.newDsl=false to the preview Gradle build.
Paparazzi 2.0.0-alpha02 still expects AGP's legacy Android extension when
creating resource preparation tasks. Disable this only when Paparazzi supports
AGP's new DSL in the target project."
  :type 'boolean
  :group 'compose-preview)

(defcustom compose-preview-open-results-after-record t
  "Whether to open generated preview images after recording succeeds."
  :type 'boolean
  :group 'compose-preview)

(defcustom compose-preview-image-width 420
  "Pixel width used for images in the Compose preview results buffer."
  :type 'integer
  :group 'compose-preview)

(defcustom compose-preview-use-android-mode-flavors t
  "Whether to reuse android-mode's module and variant discovery."
  :type 'boolean
  :group 'compose-preview)

(defvar-local compose-preview--last-module-root nil)
(defvar-local compose-preview--last-module-path nil)
(defvar-local compose-preview--last-project-root nil)
(defvar-local compose-preview--last-action nil)
(defvar-local compose-preview--last-variant nil)

(defvar compose-preview-results-buffer-name "*compose-preview-results*")

(defvar compose-preview-results-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'compose-preview-open-results)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `compose-preview-results-mode'.")

(define-derived-mode compose-preview-results-mode special-mode "ComposePreview"
  "Major mode for browsing Compose preview images."
  :group 'compose-preview)

(defun compose-preview-read-variant ()
  "Read an Android variant name for Paparazzi tasks."
  (read-string "Compose preview variant: " compose-preview-default-variant))

(defun compose-preview--find-project-root ()
  "Return the current Gradle project root."
  (when-let ((root (locate-dominating-file default-directory "gradlew")))
    (file-name-as-directory (expand-file-name root))))

(defun compose-preview--gradle-build-file-p (dir)
  "Return non-nil when DIR contains a Gradle build file."
  (or (file-exists-p (expand-file-name "build.gradle" dir))
      (file-exists-p (expand-file-name "build.gradle.kts" dir))))

(defun compose-preview--find-module-root ()
  "Return the nearest Gradle module root for `default-directory'."
  (when-let ((root (locate-dominating-file
                    default-directory
                    (lambda (dir)
                      (compose-preview--gradle-build-file-p dir)))))
    (file-name-as-directory (expand-file-name root))))

(defun compose-preview--module-path (project-root module-root)
  "Return Gradle project path for MODULE-ROOT under PROJECT-ROOT."
  (let ((relative (file-relative-name module-root project-root)))
    (if (or (string= relative "./") (string= relative "."))
        ":"
      (concat ":" (string-join (split-string (directory-file-name relative) "/" t)
                               ":")))))

(defun compose-preview--module-name (module-path)
  "Return android-mode module name for MODULE-PATH."
  (string-remove-prefix ":" module-path))

(defun compose-preview--module-root-from-name (project-root module-name)
  "Return module root under PROJECT-ROOT for android-mode MODULE-NAME."
  (file-name-as-directory
   (expand-file-name
    (replace-regexp-in-string ":" "/" module-name)
    project-root)))

(defun compose-preview--capitalize-variant (variant)
  "Return VARIANT with the first character upper-cased for Gradle task names."
  (concat (upcase (substring variant 0 1)) (substring variant 1)))

(defun compose-preview--uncapitalize-variant (variant)
  "Return VARIANT with the first character lower-cased."
  (concat (downcase (substring variant 0 1)) (substring variant 1)))

(defun compose-preview--get-init-script ()
  "Return the absolute path to the Paparazzi Gradle init script."
  (let ((base (file-name-directory
               (or load-file-name
                   (locate-library "compose-preview")
                   buffer-file-name))))
    (expand-file-name "preview.init.gradle" base)))

(defun compose-preview--gradle-executable (project-root)
  "Return the Gradle executable for PROJECT-ROOT."
  (let ((wrapper (expand-file-name "gradlew" project-root)))
    (if (file-executable-p wrapper)
        "./gradlew"
      "gradle")))

(defun compose-preview--task-path (module-path task-name)
  "Return a Gradle task path for MODULE-PATH and TASK-NAME."
  (if (string= module-path ":")
      task-name
    (concat module-path ":" task-name)))

(defun compose-preview--android-flavors-available-p ()
  "Return non-nil when android-mode flavor helpers are available."
  (and compose-preview-use-android-mode-flavors
       (fboundp 'android--get-flavors)
       (fboundp 'android--select-module)
       (fboundp 'android--select-variant)))

(defun compose-preview--android-variants (module)
  "Return android-mode variants for MODULE, or nil."
  (when (and (compose-preview--android-flavors-available-p)
             (fboundp 'android--flavor-variants))
    (ignore-errors
      (android--flavor-variants module))))

(defun compose-preview--read-variant-for-module (module force-prompt)
  "Return a variant for MODULE.
When FORCE-PROMPT is non-nil, prompt with android-mode when possible."
  (if noninteractive
      compose-preview-default-variant
    (if (compose-preview--android-flavors-available-p)
      (let ((variants (compose-preview--android-variants module)))
        (cond
         ((and (not force-prompt)
               (member compose-preview-default-variant variants))
          compose-preview-default-variant)
         ((and variants (= (length variants) 1))
          (car variants))
         (variants
          (completing-read (format "Variant (%s): " module)
                           variants nil t nil nil
                           (or (car variants) compose-preview-default-variant)))
         (t
          (compose-preview-read-variant))))
      (compose-preview-read-variant))))

(defun compose-preview--target (&optional force-prompt)
  "Return plist describing the preview target.
When FORCE-PROMPT is non-nil, prompt for module and variant via android-mode."
  (let* ((project-root (or (compose-preview--find-project-root)
                           (user-error "Could not find project root: no gradlew")))
         (module-root (or (compose-preview--find-module-root)
                          (user-error "Could not find module root: no build.gradle(.kts)")))
         (module-path (compose-preview--module-path project-root module-root))
         (module-name (compose-preview--module-name module-path)))
    (when (and force-prompt (compose-preview--android-flavors-available-p))
      (setq module-name (android--select-module)
            module-path (concat ":" module-name)
            module-root (compose-preview--module-root-from-name project-root module-name)))
    (list :project-root project-root
          :module-root module-root
          :module-path module-path
          :variant (compose-preview--read-variant-for-module module-name force-prompt))))

(defun compose-preview--run-gradle (task variant &optional action target)
  "Run Paparazzi Gradle TASK for VARIANT in the selected module.
ACTION is `record' or `verify' and is used for retrying ambiguous variants."
  (let* ((target (or target (compose-preview--target)))
         (project-root (plist-get target :project-root))
         (module-root (plist-get target :module-root))
         (module-path (plist-get target :module-path))
         (init-script (compose-preview--get-init-script))
         (task-path (compose-preview--task-path module-path task))
         (default-directory project-root)
         (args (append (list (compose-preview--gradle-executable project-root)
                             task-path)
                       (when compose-preview-disable-ksp2
                         (list "-Pksp.useKSP2=false"))
                       (when compose-preview-use-legacy-android-dsl
                         (list "-Pandroid.newDsl=false"))
                       (list "-Pksp.incremental=false"
                             "-Pkotlin.incremental=false"
                             "--no-build-cache"
                             "--no-configuration-cache"
                             "--no-parallel"
                             "--init-script"
                             init-script)))
         (command (string-join (mapcar #'shell-quote-argument args) " "))
         (process-environment
          (append (list (concat "COMPOSE_PREVIEW_MODULE_PATH=" module-path)
                        (concat "COMPOSE_PREVIEW_VARIANT=" variant)
                        (concat "COMPOSE_PREVIEW_TEMPLATE_FILE="
                                (expand-file-name
                                 "compose-preview-paparazzi-test.template.kt"
                                 (file-name-directory init-script)))
                        (concat "COMPOSE_PREVIEW_PAPARAZZI_VERSION="
                                compose-preview-paparazzi-version))
                  process-environment))
         (buffer (compilation-start
                  command
                  'compilation-mode
                  (lambda (_) "*compose-preview*"))))
    (with-current-buffer buffer
      (setq-local compose-preview--last-module-root module-root)
      (setq-local compose-preview--last-module-path module-path)
      (setq-local compose-preview--last-project-root project-root)
      (setq-local compose-preview--last-action action)
      (setq-local compose-preview--last-variant variant)
      (add-hook 'compilation-finish-functions
                #'compose-preview--compilation-finish nil t))
    (message "compose-preview: running %s in %s" task-path project-root)
    buffer))

(defun compose-preview--image-files (module-root)
  "Return Paparazzi PNG files under MODULE-ROOT."
  (let* ((roots (list (expand-file-name "src/test/snapshots" module-root)
                      (expand-file-name "build/paparazzi" module-root)
                      (expand-file-name "build/reports/paparazzi" module-root)))
         files)
    (dolist (root roots)
      (when (file-directory-p root)
        (setq files
              (nconc files
                     (directory-files-recursively root "\\.png\\'")))))
    (delete-dups (sort files #'string<))))

(defun compose-preview--candidate-variants (action)
  "Return candidate variants from the current Gradle error buffer for ACTION."
  (let ((prefix (pcase action
                  ('record "recordPaparazzi")
                  ('verify "verifyPaparazzi")
                  (_ nil)))
        (start (save-excursion
                 (goto-char (point-min))
                 (and (re-search-forward "Candidates are:" nil t)
                      (point))))
        variants)
    (when (and prefix start)
      (save-excursion
        (goto-char start)
        (while (re-search-forward
                (concat "'" (regexp-quote prefix) "\\([[:alnum:]_]+\\)'")
                nil t)
          (push (compose-preview--uncapitalize-variant (match-string 1))
                variants))))
    (delete-dups (nreverse variants))))

(defun compose-preview--retry-ambiguous-variant ()
  "Prompt for a full variant when Gradle reports an ambiguous Paparazzi task."
  (when-let* ((interactive (not noninteractive))
              (action compose-preview--last-action)
              (project-root compose-preview--last-project-root)
              (module-root compose-preview--last-module-root)
              (module-path compose-preview--last-module-path)
              (variants (compose-preview--candidate-variants action)))
    (let* ((variant (completing-read "Compose preview variant: " variants nil t
                                     nil nil (car variants)))
           (task-prefix (pcase action
                          ('record "recordPaparazzi")
                          ('verify "verifyPaparazzi")))
           (task (concat task-prefix
                         (compose-preview--capitalize-variant variant))))
      (setq compose-preview-default-variant variant)
      (message "compose-preview: retrying with variant %s" variant)
      (compose-preview--run-gradle
       task variant action
       (list :project-root project-root
             :module-root module-root
             :module-path module-path
             :variant variant)))))

(defun compose-preview--compilation-finish (buffer message)
  "Handle preview completion for BUFFER using compilation MESSAGE."
  (with-current-buffer buffer
    (let ((ambiguous-task-p (save-excursion
                              (goto-char (point-min))
                              (re-search-forward "task .* is ambiguous" nil t)))
          (success-p (string-match-p "\\(?:finished\\|exited abnormally with code 0\\)" message)))
      (cond
       (ambiguous-task-p
        (compose-preview--retry-ambiguous-variant))
       ((and compose-preview-open-results-after-record
             compose-preview--last-module-root
             (or success-p
                 (compose-preview--image-files compose-preview--last-module-root)))
        (unless success-p
          (message "compose-preview: Gradle failed after producing snapshots; opening available images"))
        (compose-preview-open-results compose-preview--last-module-root))))))

(defun compose-preview--insert-image (file)
  "Insert FILE as an image preview when Emacs can display it."
  (if (and (display-images-p)
           (image-type-available-p 'png))
      (condition-case err
          (insert-image
           (create-image file 'png nil :width compose-preview-image-width))
        (error
         (insert (format "Could not render image: %s" (error-message-string err)))))
    (insert "Image display is not available in this Emacs session.")))

(defun compose-preview--render-results (module-root images)
  "Render IMAGES for MODULE-ROOT in a preview buffer."
  (let ((buffer (get-buffer-create compose-preview-results-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (compose-preview-results-mode)
        (setq-local default-directory module-root)
        (insert (format "Compose previews: %s\n\n" module-root))
        (dolist (file images)
          (let ((relative (file-relative-name file module-root)))
            (insert-button relative
                           'follow-link t
                           'action (lambda (_button)
                                     (find-file file)))
            (insert "\n")
            (compose-preview--insert-image file)
            (insert "\n\n")))))
    (pop-to-buffer buffer)))

(defun compose-preview-open-results (&optional module-root)
  "Open generated Paparazzi preview images for MODULE-ROOT.
When called interactively, use the current Gradle module."
  (interactive)
  (let* ((root (file-name-as-directory
                (expand-file-name
                 (or module-root
                     (compose-preview--find-module-root)
                     (user-error "Could not find module root")))))
         (images (compose-preview--image-files root)))
    (cond
     ((null images)
      (message "compose-preview: no Paparazzi PNG files found under %s" root))
     (t
      (compose-preview--render-results root images)
      (message "compose-preview: found %d Paparazzi PNG files" (length images))))))

;;;###autoload
(defun compose-preview-record (&optional variant)
  "Record Compose previews for the current Android module using Paparazzi.
VARIANT defaults to `compose-preview-default-variant'."
  (interactive)
  (let* ((target (compose-preview--target current-prefix-arg))
         (variant (or variant (plist-get target :variant)))
         (task (concat "recordPaparazzi"
                       (compose-preview--capitalize-variant variant))))
    (compose-preview--run-gradle task variant 'record
                                 (plist-put target :variant variant))))

;;;###autoload
(defun compose-preview-verify (&optional variant)
  "Verify Compose previews for the current Android module using Paparazzi.
VARIANT defaults to `compose-preview-default-variant'."
  (interactive)
  (let* ((target (compose-preview--target current-prefix-arg))
         (variant (or variant (plist-get target :variant)))
         (task (concat "verifyPaparazzi"
                       (compose-preview--capitalize-variant variant))))
    (compose-preview--run-gradle task variant 'verify
                                 (plist-put target :variant variant))))

;;;###autoload
(defun compose-preview-set-variant (variant)
  "Set `compose-preview-default-variant' to VARIANT for future preview runs."
  (interactive
   (list (plist-get (compose-preview--target t) :variant)))
  (setq compose-preview-default-variant variant)
  (message "compose-preview: default variant set to %s" variant))

;;;###autoload
(defun compose-preview-gallery ()
  "Backward-compatible alias for `compose-preview-record'."
  (interactive)
  (compose-preview-record))

(provide 'compose-preview)
;;; compose-preview.el ends here
