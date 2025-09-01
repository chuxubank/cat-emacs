;;; android-mode.el --- Minor mode for Android application development

;; Copyright (C) 2009-2018 R.W van 't Veer
;; Copyright (C) 2025 Gemini (Code optimizations)

;; Author: R.W. van 't Veer
;; Created: 20 Feb 2009
;; Keywords: tools processes
;; Version: 0.6.0 (Optimized)
;; URL: https://codeberg.org/rwv/android-mode

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Provides support for running Android SDK subprocesses like the
;; emulator, logcat, ddms and ant.  When loaded `dired-mode' and
;; `find-file' hooks are added to automatically enable `android-mode'
;; when opening a file or directory in an android project.

;;; Code:

(require 'project)
(require 'cl-lib)
(require 'xml)

(defconst android-mode-default-builders
  '(ant gradle maven))

(defgroup android-mode nil
  "A minor mode for Android application development"
  :prefix "android-mode-"
  :group 'applications)

(defcustom android-mode-sdk-dir nil
  "Set to the directory containing the Android SDK.
This value will be overridden by ANDROID_HOME environment variable when
available."
  :type 'string
  :group 'android-mode)

(defcustom android-mode-sdk-tool-subdirs '("emulator" "tools" "platform-tools")
  "List of subdirectors in the SDK containing commandline tools."
  :type '(repeat string)
  :group 'android-mode)

(defcustom android-mode-sdk-tool-extensions '("" ".bat" ".exe")
  "List of possible extensions for commandline tools."
  :type '(repeat string)
  :group 'android-mode)

(defcustom android-mode-builder 'gradle
  "Builder for building an android application.
When customizing `android-mode-builder' it's important to make
sure that a corresponding entry exists in
`android-mode-root-file-plist'."
  :type 'symbol
  :options android-mode-default-builders
  :group 'android-mode)

(defcustom android-mode-root-file-plist '(ant "AndroidManifest.xml"
                                              maven "AndroidManifest.xml"
                                              gradle "gradlew")
  "Plist of mapping between different builders and the file that
  signifies the root of a project that uses that builder."
  :type '(plist :key-type symbol
                :value-type string)
  :options android-mode-default-builders
  :group 'android-mode)

(defcustom android-mode-build-command-alist
  '((ant . "ant -e")
    (maven . "mvn")
    (gradle . "./gradlew"))
  "Alist that specifies specific build command according to builder type.

Each elt has the form (BUILDER COMMAND)."
  :type '(alist :key-type symbol :value-type string)
  :options android-mode-default-builders
  :group 'android-mode)

(defcustom android-mode-key-prefix "\C-c a"
  "Minor mode keys prefix."
  :type 'string
  :group 'android-mode)

(defcustom android-mode-avd ""
  "Default AVD to use."
  :type 'string
  :group 'android-mode)

(defcustom android-mode-gradle-plugin "2.1.3"
  "Version of gradle plugin for android.
--------------------------------
Plugin version | Gradle version
--------------------------------
 1.0.0 - 1.1.3 | 2.2.1 - 2.3
 1.2.0 - 1.3.1 | 2.2.1 - 2.9
 1.5.0         | 2.2.1 - 2.13
 2.0.0 - 2.1.2 | 2.10 - 2.13
 2.1.3 - 2.2.3 | 2.14.1+
 2.3.0+        | 3.3+
--------------------------------
You need have installed Gradle version compatible with plugin,
if using the gradle wrapper and have a error try editing the distributionUrl
in YOUR_PROJECT/gradle/wrapper/gradle-wrapper.properties."
  :type 'string
  :group 'android-mode)

(defface android-mode-verbose-face '((t (:foreground "DodgerBlue")))
  "Font Lock face used to highlight VERBOSE log records."
  :group 'android-mode)

(defface android-mode-debug-face '((t (:foreground "ForestGreen")))
  "Font Lock face used to highlight DEBUG log records."
  :group 'android-mode)

(defface android-mode-info-face '((t (:foreground "Gray45")))
  "Font Lock face used to highlight INFO log records."
  :group 'android-mode)

(defface android-mode-warning-face '((t (:foreground "Red")))
  "Font Lock face used to highlight WARN log records."
  :group 'android-mode)

(defface android-mode-error-face '((t (:foreground "Red" :bold t)))
  "Font Lock face used to highlight ERROR log records."
  :group 'android-mode)

(defvar android-mode-log-face-alist
  '(("V" . android-mode-verbose-face)
    ("D" . android-mode-debug-face)
    ("I" . android-mode-info-face)
    ("W" . android-mode-warning-face)
    ("E" . android-mode-error-face)))

(defvar android-mode-log-filter-regexp ""
  "With this, user can filter output in `android-logcat-buffer'.
If received line from logcat doesn't match this, Emacs will
ignore that line.  User can see their log in a less verbose
way.")

(defvar android-mode-flavor-script
  (expand-file-name "listFlavorAppId.gradle"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Gradle init script path relative to this Emacs Lisp file.")

(defun android-root ()
  "Find the root directory of the Android project.
The root is the directory containing the project's build file
(e.g., 'gradlew' or 'AndroidManifest.xml'). Uses `locate-dominating-file`
for an efficient upward search from the current directory."
  (let ((root-file-name (plist-get android-mode-root-file-plist android-mode-builder)))
    (when root-file-name
      (locate-dominating-file default-directory root-file-name))))

(defun android-manifest-dir ()
  "Find the directory containing AndroidManifest.xml by searching upwards."
  (locate-dominating-file default-directory "AndroidManifest.xml"))

(defmacro android-in-directory (chosen-dir &rest body)
  "Execute BODY form with CHOSEN-DIR as `default-directory'.
The form is not executed when no project root directory can be found."
  `(let ((dir ,chosen-dir))
     (if dir
         (let ((default-directory dir))
           ,@body)
       (error "Can't find project root or relevant directory"))))

(defun android-local-sdk-dir ()
  "Determine the Android SDK directory.
It prioritizes in the following order:
1. `sdk.dir` from `local.properties` in the project root.
2. `ANDROID_HOME` environment variable.
3. `android-mode-sdk-dir` custom variable."
  (or
   (ignore-errors
     (android-in-directory
      (android-root)
      (let ((local-properties "local.properties"))
        (and (file-exists-p local-properties)
             (with-temp-buffer
               (insert-file-contents local-properties)
               (goto-char (point-min))
               (and (re-search-forward "^sdk\\.dir=\\(.*\\)" nil t)
                    (let ((sdk-dir (match-string 1)))
                      (and (file-directory-p sdk-dir) sdk-dir))))))))
   (getenv "ANDROID_HOME")
   android-mode-sdk-dir
   (error "No SDK directory found. Set `android-mode-sdk-dir` or ANDROID_HOME.")))

(defun android-tool-path (name)
  "Find the full path to an SDK tool NAME.
Searches in `android-mode-sdk-tool-subdirs` for the executable."
  (or (cl-loop for subdir in android-mode-sdk-tool-subdirs
               thereis (cl-loop for ext in android-mode-sdk-tool-extensions
                                for path = (expand-file-name (concat name ext)
                                                             (expand-file-name subdir (android-local-sdk-dir)))
                                when (file-exists-p path)
                                return path))
      (error "Can't find SDK tool: %s in SDK path %s" name (android-local-sdk-dir))))

(defvar android-exclusive-processes ()
  "A list of symbols representing running exclusive processes.")

(defun android-start-exclusive-command (name command &rest args)
  "Run COMMAND named NAME with ARGS unless it's already running."
  (let ((proc-name (intern name)))
    (when (not (cl-member proc-name android-exclusive-processes))
      (let* ((full-command (format "%s %s" command (mapconcat #'shell-quote-argument args " ")))
             (process (start-process-shell-command name name full-command)))
        (set-process-sentinel process
                              (lambda (proc _msg)
                                (when (memq (process-status proc) '(exit signal))
                                  (setq android-exclusive-processes
                                        (cl-remove (intern (process-name proc))
                                                   android-exclusive-processes)))))
        (push proc-name android-exclusive-processes)
        process))))

(defun android-create-project (path package activity)
  "Create new Android project on PATH with SDK app with PACKAGE and first ACTIVITY."
  (interactive "FPath: \nMPackage: \nMActivity: ")
  (let* ((target (completing-read "Target: " (android-list-targets)))
         (expanded-path (expand-file-name path))
         (base-command (format "%s create project --path %S --package %s --activity %s --target %S"
                               (android-tool-path "android")
                               expanded-path package activity target))
         (command (if (and (eq android-mode-builder 'gradle)
                           (not (equal android-mode-gradle-plugin nil)))
                      (format "%s --gradle --gradle-version %s" base-command android-mode-gradle-plugin)
                    base-command))
         (output-buffer (generate-new-buffer "*Android Create Output*"))
         (exit-code 0))
    (unwind-protect
        (progn
          (message "Running: %s" command)
          (setq exit-code (call-process-shell-command command nil output-buffer t))
          (if (/= exit-code 0)
              (with-current-buffer output-buffer
                (error "Failed to create project (exit code %d):\n%s"
                       exit-code (buffer-string)))
            (message "Project created successfully at %s" expanded-path)
            (find-file expanded-path)))
      (kill-buffer output-buffer))))

(defun android--list-sdk-items (list-arg regex &optional error-msg)
  "List Android SDK items using `android LIST-ARG`.
Parse output with REGEX, expecting one match group."
  (let* ((command (format "%s %s" (android-tool-path "android") list-arg))
         (output (shell-command-to-string command))
         (result nil)
         (offset 0))
    (while (string-match regex output offset)
      (push (match-string 1 output) result)
      (setq offset (match-end 0)))
    (if result
        (nreverse result)
      (error (or error-msg (format "No items found for command: %s" command))))))

(defun android-list-targets ()
  "List Android SDKs installed on local machine."
  (android--list-sdk-items "list target" "id: [[:digit:]]+ or \"\\(.*\\)\""
                           "No Android Targets found"))

(defun android-list-avd ()
  "List of Android Virtual Devices installed on local machine."
  (android--list-sdk-items "list avd" "Name: \\(.*\\)"
                           "No Android Virtual Devices found"))

(defun android-start-emulator ()
  "Launch Android emulator."
  (interactive)
  (let ((avd (or (and (not (string-blank-p android-mode-avd)) android-mode-avd)
                 (completing-read "Android Virtual Device: " (android-list-avd)))))
    (unless (android-start-exclusive-command (format "*android-emulator-%s*" avd)
                                             (android-tool-path "emulator")
                                             "-avd"
                                             avd)
      (message "Emulator for %s is already running or being started." avd))))

(defun android-start-ddms ()
  "Launch Dalvik Debug Monitor Service tool."
  (interactive)
  (unless (android-start-exclusive-command "*android-ddms*" (android-tool-path "ddms"))
    (message "ddms already running")))

(defcustom android-logcat-buffer "*android-logcat*"
  "Name for the buffer where logcat output goes."
  :type 'string
  :group 'android-mode)

(defun android-logcat-find-file ()
  "Open file at point in logcat."
  (interactive)
  (let ((filename (get-text-property (point) 'filename))
        (linenr (get-text-property (point) 'linenr))
        (root (android-root)))
    (when (and filename root)
      (find-file (expand-file-name (concat "src/" filename) root))
      (goto-char (point-min))
      (forward-line (1- linenr)))))

(defun android-logcat-find-file-mouse (event)
  "Open file at mouse EVENT in logcat."
  (interactive "e")
  (with-selected-window (posn-window (event-end event))
    (goto-char (posn-point (event-end event)))
    (android-logcat-find-file)))

(defvar android-logcat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'android-logcat-find-file)
    (define-key map [mouse-2] 'android-logcat-find-file-mouse)
    (define-key map (kbd "n") 'next-logical-line)
    (define-key map (kbd "p") 'previous-logical-line)
    (define-key map (kbd "q") 'delete-window)
    (define-key map (kbd "f") 'android-logcat-set-filter)
    (define-key map (kbd "c") 'android-logcat-clear-filter)
    (define-key map (kbd "C") 'android-logcat-erase-buffer)
    map))

(defun android-logcat-prepare-msg (msg)
  "Apply text properties to log MSG."
  (if (string-match "\\bat \\(.+\\)\\.\\([^.]+\\)\\.\\([^.]+\\)(\\(.+\\):\\([0-9]+\\))" msg)
      (let* ((package (match-string 1 msg))
             (class (match-string 2 msg))
             (method (match-string 3 msg))
             (filename (concat (replace-regexp-in-string "\\." "/" package) "/" (match-string 4 msg)))
             (linenr (match-string 5 msg))
             (root (android-root)))
        (if (and root (file-exists-p (expand-file-name (concat "src/" filename) root)))
            (propertize msg
                        'face 'underline
                        'mouse-face 'highlight
                        'filename filename
                        'linenr (string-to-number linenr)
                        'follow-link t)
          msg))
    msg))

(defvar android-logcat-pending-output ""
  "Buffer for incomplete lines from the logcat process filter.")

(defun android-logcat-process-filter (_process output)
  "Process filter for logcat, displaying styled OUTPUT.
Handles partial lines by buffering them in `android-logcat-pending-output'."
  (with-current-buffer android-logcat-buffer
    (let ((following (= (point) (point-max))) ; Was the point at the end of the buffer?
          (buffer-read-only nil)
          (pos 0)
          ;; Concatenate pending output with new output, normalizing line endings.
          (output (concat android-logcat-pending-output
                          (replace-regexp-in-string "\r" "" output))))
      (save-excursion
        (goto-char (point-max))
        ;; Process each full line.
        (while (string-match "\n" output pos)
          (let ((line (substring output pos (match-beginning 0))))
            (setq pos (match-end 0))
            (when (string-match android-mode-log-filter-regexp line)
              (if (string-match "^\\(.\\)/\\(.*\\)( *\\([0-9]+\\)): \\(.*\\)$" line)
                  (let* ((level (match-string 1 line))
                         (level-face (cdr (or (assoc level android-mode-log-face-alist)
                                              (assoc "I" android-mode-log-face-alist))))
                         (tag (replace-regexp-in-string " *$" "" (match-string 2 line)))
                         (pid (match-string 3 line))
                         (msg (match-string 4 line)))
                    (insert (propertize level 'face level-face))
                    (tab-to-tab-stop)
                    (insert (propertize tag 'face 'font-lock-function-name-face))
                    (insert (propertize (concat "("  pid ")") 'face 'font-lock-constant-face))
                    (tab-to-tab-stop)
                    (insert (android-logcat-prepare-msg (propertize msg 'face level-face))))
                (insert (propertize line 'face 'font-lock-warning-face)))
              (insert "\n"))))
        ;; Store any remaining partial line.
        (setq android-logcat-pending-output (substring output pos)))
      (when following (goto-char (point-max))))))

(defun android-logcat ()
  "Switch to ADB logcat buffer, creating it if it doesn't exist."
  (interactive)
  (when (android-start-exclusive-command android-logcat-buffer
                                         (android-tool-path "adb")
                                         "logcat")
    (set-process-filter (get-buffer-process android-logcat-buffer)
                        #'android-logcat-process-filter)
    (with-current-buffer (get-buffer-create android-logcat-buffer)
      (setq buffer-read-only t)
      (set (make-local-variable 'tab-stop-list) '(2 30))
      (set (make-local-variable 'android-mode-log-filter-regexp) "")
      (use-local-map android-logcat-map)
      (font-lock-mode t)
      (android-mode t)))
  (switch-to-buffer android-logcat-buffer)
  (goto-char (point-max)))

(defun android-current-buffer-class-name ()
  "Try to determine the fully qualified class name defined in the current buffer."
  (save-excursion
    (when (and buffer-file-name (string-match "\\.java$" buffer-file-name))
      (goto-char (point-min))
      (let ((case-fold-search nil)
            package class)
        (when (re-search-forward "^[ \t]*package[ \t]+\\([a-z0-9_.]+\\);" nil t)
          (setq package (match-string-no-properties 1)))
        (goto-char (point-min))
        (when (re-search-forward "\\bpublic[ \t]+\\(?:class\\|interface\\|enum\\)[ \t]+\\([A-Za-z0-9_]+\\)" nil t)
          (setq class (match-string-no-properties 1)))
        (cond ((and package class) (concat package "." class))
              (class class))))))

(defun android-project-package ()
  "Return the package of the Android project."
  (android-in-directory
   (android-manifest-dir)
   (let ((root (car (xml-parse-file "AndroidManifest.xml"))))
     (xml-get-attribute root 'package))))

(defun android-project-main-activities (&optional category)
  "Return list of main activity class names as found in the manifest.
The names returned are fully qualified class names.
Names starting with a period or a capital letter are prepended by
the project package name.

Filter on CATEGORY intent when supplied."
  (android-in-directory
   (android-manifest-dir)
   (cl-flet* ((first-xml-child (parent name)
                (car (xml-get-children parent name)))
              (action-main-p (activity)
                (let ((el (first-xml-child (first-xml-child activity 'intent-filter) 'action)))
                  (equal "android.intent.action.MAIN"
                         (xml-get-attribute el 'android:name))))
              (category-p (activity)
                (some (lambda (cat-el)
                        (equal (concat "android.intent.category." category)
                               (xml-get-attribute cat-el 'android:name)))
                      (xml-get-children (first-xml-child activity 'intent-filter) 'category))))
     (let* ((root (car (xml-parse-file "AndroidManifest.xml")))
            (package (xml-get-attribute root 'package))
            (application (first-xml-child root 'application))
            (activities (xml-get-children application 'activity)))
       (mapcar (lambda (activity)
                 (let ((case-fold-search nil)
                       (name (xml-get-attribute activity 'android:name)))
                   (cond ((string-prefix-p "." name) (concat package name))
                         ((string-match "^[A-Z]" name) (concat package "." name))
                         (t name))))
               (cl-remove-if-not (lambda (activity)
                                   (and (action-main-p activity)
                                        (or (not category) (category-p activity))))
                                 activities))))))

(defun android-start-app ()
  "Start activity in the running emulator.
When the current buffer holds an activity class specified in the
manifest as a main action intent it will be run.  Otherwise
start the first activity in the 'LAUNCHER' category."
  (interactive)
  (let* ((package (android-project-package))
         (current (android-current-buffer-class-name))
         (main-activities (android-project-main-activities))
         (launcher-activity (car (android-project-main-activities "LAUNCHER")))
         (activity (or (and (member current main-activities) current)
                       launcher-activity)))
    (unless activity (error "No main launcher activity found in manifest"))
    (message "Starting activity: %s" activity)
    (let* ((command (format "%s shell am start -n %s/%s"
                            (android-tool-path "adb") package activity))
           (output (shell-command-to-string command)))
      (when (string-match-p "^Error: " output)
        (error "Error starting app:\n%s" output)))))

(defun android-logcat-set-filter (regexp-filter)
  "Set the filter of `android-logcat-buffer' to REGEXP-FILTER."
  (interactive "MRegexp Filter: ")
  (with-current-buffer android-logcat-buffer
    (let ((buffer-read-only nil)
          (info-face (cdr (assoc "I" android-mode-log-face-alist)))
          msg)
      (goto-char (point-max))
      (if (string-blank-p regexp-filter)
          (setq msg "\n\n*** Filter is cleared ***\n\n")
        (setq msg (format "\n\n*** Filter is changed to '%s' ***\n\n" regexp-filter)))
      (insert (propertize msg 'face info-face))))
  (setq android-mode-log-filter-regexp regexp-filter))

(defun android-logcat-clear-filter ()
  "Clear the filter of `android-logcat-buffer'."
  (interactive)
  (android-logcat-set-filter ""))

(defun android-logcat-erase-buffer ()
  "Clear the contents of the logcat buffer."
  (interactive)
  (with-current-buffer android-logcat-buffer
    (let ((buffer-read-only nil))
      (erase-buffer))))

(defun android-print-flavor ()
  "Print the project's flavors, variants and application IDs."
  (interactive)
  (android-in-directory
   (android-root)
   (let* ((script android-mode-flavor-script)
          (command (format "./gradlew -I %s --quiet" script))
          (output (shell-command-to-string command))
          (flavors (android-parse-gradle-flavors output)))
     (dolist (f flavors)
       (message "Module: %s Variant: %s AppId: %s"
                (nth 0 f) (nth 1 f) (nth 2 f))))))

(defun android-parse-gradle-flavors (gradle-output)
  "Parse GRADLE-OUTPUT and return a list of (MODULE VARIANT APPID) tuples.
Only considers lines between ===FLAVORS_START=== and ===FLAVORS_END===."
  (let ((in-flavors nil)
        (result '()))
    (dolist (line (split-string gradle-output "\n" t))
      (cond
       ((string-match-p "===FLAVORS_START===" line)
        (setq in-flavors t))
       ((string-match-p "===FLAVORS_END===" line)
        (setq in-flavors nil))
       (in-flavors
        (when (string-match "^\\([^:]+\\):\\([^=]+\\)=\\(.+\\)$" line)
          (let ((module (match-string 1 line))
                (variant (match-string 2 line))
                (appid (match-string 3 line)))
            (push (list module variant appid) result))))))
    (nreverse result)))

(defmacro android-defun-builder (builder)
  `(defun ,(intern (concat "android-" builder)) (tasks-or-goals)
     ,(concat "Run " builder " TASKS-OR-GOALS in the project root directory.")
     (interactive "sTasks or Goals: ")
     (android-in-directory
      (android-root)
      (compile (format "%s %s"
                       (cdr (assoc ',(intern builder) android-mode-build-command-alist))
                       tasks-or-goals)))))

(android-defun-builder "ant")
(android-defun-builder "maven")
(android-defun-builder "gradle")

;; Ant
(defmacro android-defun-ant-task (task)
  `(defun ,(intern (concat "android-ant-"
                           (replace-regexp-in-string "[[:space:]]" "-" task)))
       ()
     ,(concat "Run 'ant " task "' in the project root directory.")
     (interactive)
     (android-ant ,task)))

(android-defun-ant-task "clean")
(android-defun-ant-task "test")
(android-defun-ant-task "debug")
(android-defun-ant-task "installd")
(android-defun-ant-task "uninstall")

;; Maven
(defmacro android-defun-maven-task (task)
  `(defun ,(intern (concat "android-maven-"
                           (replace-regexp-in-string "[[:space:]:]" "-" task)))
       ()
     ,(concat "Run maven " task " in the project root directory.")
     (interactive)
     (android-maven ,task)))

(android-defun-maven-task "clean")
(android-defun-maven-task "test")
(android-defun-maven-task "install")
(android-defun-maven-task "android:deploy")
(android-defun-maven-task "android:redeploy")
(android-defun-maven-task "android:undeploy")

;; Gradle
(defmacro android-defun-gradle-task (task)
  `(defun ,(intern (concat "android-gradle-"
                           (replace-regexp-in-string "[[:space:]:]" "-" task)))
       ()
     ,(concat "Run gradle " task " in the project root directory.")
     (interactive)
     (android-gradle ,task)))

(android-defun-gradle-task "clean")
(android-defun-gradle-task "test")
(android-defun-gradle-task "assembleDebug")
(android-defun-gradle-task "assembleRelease")
(android-defun-gradle-task "installDebug")
(android-defun-gradle-task "uninstallDebug")

;; Common build functions
(defun android-build-clean ()
  "Remove output files created by building."
  (interactive)
  (funcall (case android-mode-builder
             ('ant 'android-ant-clean)
             ('gradle 'android-gradle-clean)
             ('maven 'android-maven-clean))))

(defun android-build-test ()
  "Run the tests."
  (interactive)
  (funcall (case android-mode-builder
             ('ant 'android-ant-test)
             ('gradle 'android-gradle-test)
             ('maven 'android-maven-test))))

(defun android-build-debug ()
  "Build the application in a debug mode."
  (interactive)
  (funcall (case android-mode-builder
             ('ant 'android-ant-debug)
             ('gradle 'android-gradle-assembleDebug)
             ('maven 'android-maven-install))))

(defun android-build-install ()
  "Install a generated apk file to the device."
  (interactive)
  (funcall (case android-mode-builder
             ('ant 'android-ant-installd)
             ('gradle 'android-gradle-installDebug)
             ('maven 'android-maven-android-deploy))))

(defun android-build-reinstall ()
  "Reinstall a generated apk file to the device."
  (interactive)
  (funcall (case android-mode-builder
             ('maven 'android-maven-android-redeploy)
             (t (error "%s builder does not support reinstall"
                       android-mode-builder)))))

(defun android-build-uninstall ()
  "Uninstall a generated apk file from the device."
  (interactive)
  (funcall (case android-mode-builder
             ('ant 'android-ant-uninstall)
             ('gradle 'android-gradle-uninstallDebug)
             ('maven 'android-maven-android-undeploy))))

(defconst android-mode-keys
  '(("d" . android-start-ddms)
    ("e" . android-start-emulator)
    ("l" . android-logcat)
    ("C" . android-build-clean)
    ("t" . android-build-test)
    ("c" . android-build-debug)
    ("i" . android-build-install)
    ("r" . android-build-reinstall)
    ("u" . android-build-uninstall)
    ("a" . android-start-app)))

(defvar android-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (spec android-mode-keys)
      (define-key map
                  (read-kbd-macro (concat android-mode-key-prefix " " (car spec)))
                  (cdr spec)))
    map))

;;;###autoload
(define-minor-mode android-mode
  "Android application development minor mode."
  :lighter " Android"
  :keymap android-mode-map)

(defun android-mode-enable-if-project ()
  "Enable `android-mode' if the current file is in an Android project."
  (when (android-root)
    (android-mode 1)))

(add-hook 'dired-mode-hook #'android-mode-enable-if-project)
(add-hook 'find-file-hook #'android-mode-enable-if-project)

(provide 'android-mode)

;;; android-mode.el ends here
