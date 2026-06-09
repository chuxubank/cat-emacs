;;; compose-preview.el --- Paparazzi previews for Jetpack Compose -*- lexical-binding: t; -*-

;;; Commentary:

;; Generate a temporary Paparazzi-backed preview runner for the current Android
;; module, refresh rendered @Preview images, and show them in Emacs.

;;; Code:

(require 'cl-lib)
(require 'compile)
(require 'seq)
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
  "Whether to open generated preview images after refresh or record succeeds."
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

(defcustom compose-preview-force-clean-build nil
  "Whether preview refresh should disable Gradle, Kotlin and KSP caches.
This is slower, but can be useful when a project has stale generated state."
  :type 'boolean
  :group 'compose-preview)

(defvar-local compose-preview--last-module-root nil)
(defvar-local compose-preview--last-module-path nil)
(defvar-local compose-preview--last-project-root nil)
(defvar-local compose-preview--last-action nil)
(defvar-local compose-preview--last-variant nil)
(defvar-local compose-preview--last-source-file nil)
(defvar-local compose-preview--last-source-previews nil)

(defvar compose-preview-results-buffer-name "*compose-preview-results*")
(defvar compose-preview-log-buffer-name "*compose-preview-log*")
(defvar compose-preview--target-cache nil
  "Project-level target cache.
Each entry is (PROJECT-ROOT . TARGET), where TARGET is a plist containing
:project-root, :module-root, :module-path and :variant.")

(cl-defstruct compose-preview-item
  id
  declaring-class
  method
  name
  preview-name
  group
  source-file
  files)

(defvar compose-preview-results-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'compose-preview-refresh)
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

(defun compose-preview--sanitize (value)
  "Return Gradle-side sanitized VALUE."
  (replace-regexp-in-string "[^A-Za-z0-9_]" "_" value))

(defun compose-preview--cache-key (project-root)
  "Return normalized cache key for PROJECT-ROOT."
  (directory-file-name (expand-file-name project-root)))

(defun compose-preview--cached-target (project-root)
  "Return cached preview target for PROJECT-ROOT."
  (cdr (assoc (compose-preview--cache-key project-root)
              compose-preview--target-cache)))

(defun compose-preview--cache-target (target)
  "Cache TARGET for its project root and return TARGET."
  (let* ((project-root (plist-get target :project-root))
         (key (compose-preview--cache-key project-root))
         (entry (assoc key compose-preview--target-cache)))
    (if entry
        (setcdr entry target)
      (push (cons key target) compose-preview--target-cache))
    target))

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
        wrapper
      "gradle")))

(defun compose-preview--task-path (module-path task-name)
  "Return a Gradle task path for MODULE-PATH and TASK-NAME."
  (if (string= module-path ":")
      task-name
    (concat module-path ":" task-name)))

(defun compose-preview--current-package ()
  "Return Kotlin package name in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           "^[[:space:]]*package[[:space:]]+\\([A-Za-z_][A-Za-z0-9_.]*\\)"
           nil t)
      (match-string-no-properties 1))))

(defun compose-preview--current-buffer-class-prefix ()
  "Return scanner declaring class prefix for the current Kotlin buffer."
  (when-let* ((file buffer-file-name)
              ((string-match-p "\\.kt\\'" file)))
    (let ((package (compose-preview--current-package))
          (facade (concat (file-name-base file) "Kt")))
      (if (and package (not (string-empty-p package)))
          (concat package "." facade)
        facade))))

(defun compose-preview--source-file-class-prefix (file)
  "Return scanner declaring class prefix for Kotlin source FILE."
  (when (and file (file-readable-p file) (string-match-p "\\.kt\\'" file))
    (with-temp-buffer
      (insert-file-contents file)
      (setq-local buffer-file-name file)
      (compose-preview--current-buffer-class-prefix))))

(defun compose-preview--manifest-file (target)
  "Return scanner manifest file path for TARGET."
  (expand-file-name
   (format ".gradle/compose-preview/%s/%s/previews.tsv"
           (compose-preview--sanitize (plist-get target :module-path))
           (plist-get target :variant))
   (plist-get target :project-root)))

(defun compose-preview--unescape-field (value)
  "Unescape scanner manifest field VALUE."
  (let ((result value))
    (setq result (replace-regexp-in-string "\\\\r" "\r" result t t))
    (setq result (replace-regexp-in-string "\\\\n" "\n" result t t))
    (setq result (replace-regexp-in-string "\\\\t" "\t" result t t))
    (setq result (replace-regexp-in-string "\\\\\\\\" "\\\\" result t t))
    result))

(defun compose-preview--read-manifest (target)
  "Read scanner preview manifest for TARGET."
  (let ((file (compose-preview--manifest-file target))
        items)
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (dolist (line (split-string (buffer-string) "\n" t))
          (pcase-let ((`(,id ,display-name ,declaring-class ,method ,preview-name ,group ,source-file)
                       (mapcar #'compose-preview--unescape-field
                               (split-string line "\t"))))
            (push (make-compose-preview-item
                   :id id
                   :name display-name
                   :declaring-class declaring-class
                   :method method
                   :preview-name preview-name
                   :group group
                   :source-file source-file)
                  items)))))
    (nreverse items)))

(defun compose-preview--same-file-p (left right)
  "Return non-nil when LEFT and RIGHT name the same source file."
  (and left right
       (not (string-empty-p left))
       (not (string-empty-p right))
       (string= (file-truename left)
                (file-truename right))))

(defun compose-preview--current-buffer-previews (&optional target)
  "Return scanner manifest previews for the current Kotlin buffer and TARGET."
  (when-let* ((target (or target (ignore-errors (compose-preview--target))))
              (file buffer-file-name))
    (seq-filter
     (lambda (item)
       (compose-preview--same-file-p file
                                     (compose-preview-item-source-file item)))
     (compose-preview--read-manifest target))))

(defun compose-preview--source-file-previews (file target)
  "Return scanner manifest previews for Kotlin source FILE and TARGET."
  (when file
    (seq-filter
     (lambda (item)
       (compose-preview--same-file-p file
                                     (compose-preview-item-source-file item)))
     (compose-preview--read-manifest target))))

(defun compose-preview--snapshot-stem (file)
  "Return FILE basename without PNG extension."
  (file-name-sans-extension (file-name-nondirectory file)))

(defun compose-preview--snapshot-matches-preview-p (file preview)
  "Return non-nil when snapshot FILE belongs to PREVIEW."
  (let* ((stem (compose-preview--snapshot-stem file))
         (method-prefix (concat
                         (compose-preview-item-declaring-class preview)
                         "_"
                         (compose-preview-item-method preview))))
    (and (string-match-p (regexp-quote method-prefix) stem)
         (string-match-p (regexp-quote (compose-preview-item-id preview)) stem))))

(defun compose-preview--report-run-files (module-root)
  "Return Paparazzi report run metadata files under MODULE-ROOT."
  (let ((reports-root (expand-file-name "build/reports/paparazzi" module-root))
        files)
    (when (file-directory-p reports-root)
      (dolist (runs-dir (directory-files-recursively reports-root "\\`runs\\'" t))
        (when (file-directory-p runs-dir)
          (setq files
                (nconc files
                       (directory-files-recursively runs-dir "\\.js\\'"))))))
    (sort files #'string<)))

(defun compose-preview--report-name-base (name)
  "Return Paparazzi run NAME without a duplicate numeric suffix."
  (if (string-match "\\`\\(.*\\)_[0-9]+\\'" name)
      (match-string 1 name)
    name))

(defun compose-preview--put-report-image (index key file)
  "Add FILE to report INDEX under KEY."
  (puthash key (cons file (gethash key index)) index))

(defun compose-preview--report-js-field (field)
  "Return Paparazzi run JS FIELD value from the current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           (format "\"%s\"[[:space:]]*:[[:space:]]*\"\\([^\"]*\\)\""
                   (regexp-quote field))
           nil t)
      (match-string-no-properties 1))))

(defun compose-preview--report-image-index (module-root)
  "Return hash table from preview keys to report PNG files."
  (let ((index (make-hash-table :test #'equal)))
    (dolist (run-file (compose-preview--report-run-files module-root))
      (with-temp-buffer
        (insert-file-contents run-file)
        (let* ((name (compose-preview--report-js-field "name"))
               (test-name (compose-preview--report-js-field "testName"))
               (image (compose-preview--report-js-field "file"))
               (run-root (file-name-directory (directory-file-name
                                               (file-name-directory run-file)))))
          (when (and test-name image
                     (string-match
                      "#snapshot\\[[0-9]+\\.\\(.+\\)_\\([^]]+\\)\\]"
                      test-name))
            (let* ((declaring-class (match-string 1 test-name))
                   (method (match-string 2 test-name))
                   (file (expand-file-name image run-root)))
              (when (file-readable-p file)
                (compose-preview--put-report-image
                 index
                 (concat declaring-class "#" method)
                 file)
                (when (and name (not (string-empty-p name)))
                  (compose-preview--put-report-image
                   index
                   (concat declaring-class
                           "#"
                           method
                           "#"
                           (compose-preview--report-name-base name))
                   file))))))))
    (maphash (lambda (key value)
               (puthash key (delete-dups (sort value #'string<)) index))
             index)
    index))

(defun compose-preview--report-files-for-preview (preview report-index)
  "Return report PNG files for PREVIEW from REPORT-INDEX."
  (let ((method-key (concat (compose-preview-item-declaring-class preview)
                            "#"
                            (compose-preview-item-method preview)))
        (id (compose-preview-item-id preview)))
    (or (and id
             (not (string-empty-p id))
             (gethash (concat method-key "#" id) report-index))
        (gethash method-key report-index))))

(defun compose-preview--attach-preview-files (previews files module-root)
  "Return PREVIEWS with matching snapshot FILES under MODULE-ROOT attached."
  (let ((report-index (compose-preview--report-image-index module-root)))
    (mapcar
     (lambda (preview)
       (setf (compose-preview-item-files preview)
             (or (and report-index
                      (compose-preview--report-files-for-preview preview report-index))
                 (seq-filter (lambda (file)
                               (compose-preview--snapshot-matches-preview-p file preview))
                             files)))
       preview)
     previews)))

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
         (cached (compose-preview--cached-target project-root)))
    (if (and cached (not force-prompt))
        cached
      (let* ((module-path (compose-preview--module-path project-root module-root))
             (module-name (compose-preview--module-name module-path)))
        (when (and force-prompt (compose-preview--android-flavors-available-p))
          (setq module-name (android--select-module)
                module-path (concat ":" module-name)
                module-root (compose-preview--module-root-from-name project-root module-name)))
        (compose-preview--cache-target
         (list :project-root project-root
               :module-root module-root
               :module-path module-path
               :variant (compose-preview--read-variant-for-module module-name force-prompt)))))))

(defun compose-preview--gradle-context (task variant target)
  "Return plist for running Gradle TASK for VARIANT and TARGET."
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
                       (when compose-preview-force-clean-build
                         (list "-Pksp.incremental=false"
                               "-Pkotlin.incremental=false"
                               "--no-build-cache"
                               "--no-configuration-cache"
                               "--no-parallel"))
                       (list "--init-script"
                             init-script)))
         (command (string-join (mapcar #'shell-quote-argument args) " "))
         (env (list (concat "COMPOSE_PREVIEW_MODULE_PATH=" module-path)
                    (concat "COMPOSE_PREVIEW_VARIANT=" variant)
                    (concat "COMPOSE_PREVIEW_SOURCE_FILE="
                            (or (plist-get target :source-file) ""))
                    (concat "COMPOSE_PREVIEW_TEMPLATE_FILE="
                            (expand-file-name
                             "compose-preview-paparazzi-test.template.kt"
                             (file-name-directory init-script)))
                    (concat "COMPOSE_PREVIEW_PAPARAZZI_VERSION="
                            compose-preview-paparazzi-version))))
    (list :project-root project-root
          :module-root module-root
          :module-path module-path
          :variant variant
          :task-path task-path
          :args args
          :command command
          :env env)))

(defun compose-preview--run-gradle (task variant &optional action target)
  "Run Paparazzi Gradle TASK for VARIANT visibly in a compilation buffer.
ACTION is `record' or `verify' and is used for retrying ambiguous variants."
  (let* ((context (compose-preview--gradle-context task variant target))
         (project-root (plist-get context :project-root))
         (module-root (plist-get context :module-root))
         (module-path (plist-get context :module-path))
         (task-path (plist-get context :task-path))
         (default-directory project-root)
         (process-environment
          (append (plist-get context :env) process-environment))
         (buffer (compilation-start
                  (plist-get context :command)
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

(defun compose-preview--run-gradle-silent (task variant &optional action target)
  "Run Paparazzi Gradle TASK for VARIANT silently for preview refresh."
  (let* ((context (compose-preview--gradle-context task variant target))
         (project-root (plist-get context :project-root))
         (module-root (plist-get context :module-root))
         (module-path (plist-get context :module-path))
         (source-file (plist-get target :source-file))
         (buffer (get-buffer-create compose-preview-log-buffer-name))
         (default-directory project-root)
         (process-environment
          (append (plist-get context :env) process-environment)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "$ %s\n\n" (plist-get context :command)))))
    (message "Refreshing Compose previews...")
    (make-process
     :name "compose-preview-refresh"
     :buffer buffer
     :command (plist-get context :args)
     :noquery t
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (let ((exit-code (process-exit-status proc)))
           (with-current-buffer (process-buffer proc)
             (setq-local compose-preview--last-module-root module-root)
             (setq-local compose-preview--last-module-path module-path)
             (setq-local compose-preview--last-project-root project-root)
             (setq-local compose-preview--last-action action)
             (setq-local compose-preview--last-variant variant)
             (setq-local compose-preview--last-source-file source-file)
             (setq-local compose-preview--last-source-previews nil))
           (cond
            ((zerop exit-code)
             (compose-preview-open-results
              module-root
              (compose-preview--source-file-previews source-file target)
              source-file))
            ((with-current-buffer (process-buffer proc)
               (save-excursion
                 (goto-char (point-min))
                 (re-search-forward "task .* is ambiguous" nil t)))
             (with-current-buffer (process-buffer proc)
               (compose-preview--retry-ambiguous-variant)))
            ((compose-preview--image-files module-root)
             (message "compose-preview: Gradle reported an error after rendering; opening available previews")
             (compose-preview-open-results
              module-root
              (compose-preview--source-file-previews source-file target)
              source-file))
            (t
             (display-buffer (process-buffer proc))
             (message "compose-preview: refresh failed; see %s" compose-preview-log-buffer-name)))))))
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
  (let* ((task-shape (pcase action
                       ('preview '("test" . "UnitTest"))
                       ('record '("recordPaparazzi" . ""))
                       ('verify '("verifyPaparazzi" . ""))
                       (_ nil)))
         (prefix (car task-shape))
         (suffix (cdr task-shape))
        (start (save-excursion
                 (goto-char (point-min))
                 (and (re-search-forward "Candidates are:" nil t)
                      (point))))
        variants)
    (when (and prefix start)
      (save-excursion
        (goto-char start)
        (while (re-search-forward
                (concat "'" (regexp-quote prefix)
                        "\\([[:alnum:]_]+\\)"
                        (regexp-quote suffix)
                        "'")
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
                          ('preview "test")
                          ('record "recordPaparazzi")
                          ('verify "verifyPaparazzi")))
           (task (concat task-prefix
                         (compose-preview--capitalize-variant variant)
                         (when (eq action 'preview)
                           "UnitTest"))))
      (setq compose-preview-default-variant variant)
      (message "compose-preview: retrying with variant %s" variant)
      (let ((target (list :project-root project-root
                          :module-root module-root
                          :module-path module-path
                          :variant variant)))
        (compose-preview--cache-target target)
        (if (eq action 'preview)
            (compose-preview--run-gradle-silent task variant action target)
          (compose-preview--run-gradle task variant action target))))))

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

(defun compose-preview--render-results (module-root images &optional previews)
  "Render IMAGES for MODULE-ROOT in a preview buffer.
When PREVIEWS is non-nil, render by preview display name instead of file name."
  (let ((buffer (get-buffer-create compose-preview-results-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (compose-preview-results-mode)
        (setq-local default-directory module-root)
        (insert (format "Compose Preview  %s\n\n" module-root))
        (if previews
            (dolist (preview previews)
              (let ((files (compose-preview-item-files preview)))
                (when files
                  (insert (propertize (compose-preview-item-name preview)
                                      'face 'bold)
                          "\n")
                  (dolist (file files)
                    (insert-button "open image"
                                   'follow-link t
                                   'action (lambda (_button)
                                             (find-file file)))
                    (insert "\n")
                    (compose-preview--insert-image file)
                    (insert "\n"))
                  (insert "\n"))))
          (dolist (file images)
            (let ((relative (file-relative-name file module-root)))
              (insert-button relative
                             'follow-link t
                             'action (lambda (_button)
                                       (find-file file)))
              (insert "\n")
              (compose-preview--insert-image file)
              (insert "\n\n"))))))
    (pop-to-buffer buffer)))

(defun compose-preview-open-results (&optional module-root previews source-file)
  "Open generated Paparazzi preview images for MODULE-ROOT.
When called interactively, use the current Gradle module."
  (interactive)
  (let* ((root (file-name-as-directory
                (expand-file-name
                 (or module-root
                     (compose-preview--find-module-root)
                     (user-error "Could not find module root")))))
         (images (compose-preview--image-files root))
         (source-file (or source-file
                          (and buffer-file-name
                               (string-match-p "\\.kt\\'" buffer-file-name)
                               buffer-file-name)))
         (previews (or previews
                       (and source-file
                            (compose-preview--source-file-previews
                             source-file
                             (ignore-errors (compose-preview--target))))))
         (preview-images (and previews
                              (compose-preview--attach-preview-files
                               previews images root)))
         (visible-images (if preview-images
                             (apply #'append
                                    (mapcar #'compose-preview-item-files preview-images))
                           (unless source-file
                             images))))
    (cond
     ((and source-file (null previews))
      (message "compose-preview: no scanner previews found for %s"
               (file-name-nondirectory source-file)))
     ((null visible-images)
      (if source-file
          (message "compose-preview: no rendered preview PNGs found for %s"
                   (file-name-nondirectory source-file))
        (message "compose-preview: no Paparazzi PNG files found under %s" root)))
     (t
      (compose-preview--render-results root visible-images preview-images)
      (message "compose-preview: found %d Paparazzi PNG files" (length visible-images))))))

;;;###autoload
(defun compose-preview-refresh (&optional variant)
  "Refresh Android Studio-style Compose previews for the current module.
VARIANT defaults to the selected android-mode variant or
`compose-preview-default-variant'.  With a prefix argument, prompt for module
and variant using android-mode's flavor data when available."
  (interactive)
  (let* ((target (compose-preview--target current-prefix-arg))
         (variant (or variant (plist-get target :variant)))
         (source-file buffer-file-name)
         (task (concat "test"
                       (compose-preview--capitalize-variant variant)
                       "UnitTest")))
    (setq target (compose-preview--cache-target
                  (plist-put target :variant variant)))
    (setq target (plist-put target :source-file source-file))
    (compose-preview--run-gradle-silent task variant 'preview
                                        target)))

;;;###autoload
(defun compose-preview-record (&optional variant)
  "Record Paparazzi snapshots for the current Android module.
VARIANT defaults to `compose-preview-default-variant'."
  (interactive)
  (let* ((target (compose-preview--target current-prefix-arg))
         (variant (or variant (plist-get target :variant)))
         (task (concat "recordPaparazzi"
                       (compose-preview--capitalize-variant variant))))
    (setq target (compose-preview--cache-target
                  (plist-put target :variant variant)))
    (compose-preview--run-gradle task variant 'record
                                 target)))

;;;###autoload
(defun compose-preview-verify (&optional variant)
  "Verify Paparazzi snapshots for the current Android module.
VARIANT defaults to `compose-preview-default-variant'."
  (interactive)
  (let* ((target (compose-preview--target current-prefix-arg))
         (variant (or variant (plist-get target :variant)))
         (task (concat "verifyPaparazzi"
                       (compose-preview--capitalize-variant variant))))
    (setq target (compose-preview--cache-target
                  (plist-put target :variant variant)))
    (compose-preview--run-gradle task variant 'verify
                                 target)))

;;;###autoload
(defun compose-preview-set-variant (variant)
  "Set `compose-preview-default-variant' to VARIANT for future preview runs."
  (interactive
   (list (plist-get (compose-preview--target t) :variant)))
  (setq compose-preview-default-variant variant)
  (message "compose-preview: default variant set to %s" variant))

(provide 'compose-preview)
;;; compose-preview.el ends here
