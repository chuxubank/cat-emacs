;;; -*- lexical-binding: t; -*-
;;; android-mode.el --- Minor mode for Android application development

;; Copyright (C) 2009-2018 R.W van 't Veer
;; Copyright (C) 2025 Gemini (Code optimizations and warning fixes)

;; Author: R.W. van 't Veer
;; Created: 20 Feb 2009
;; Keywords: tools processes
;; Version: 0.7.0 (Optimized & Warnings Fixed)
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
;; emulator, build and install tasks.  When loaded `dired-mode' and
;; `find-file' hooks are added to automatically enable `android-mode'
;; when opening a file or directory in an android project.

;;; Code:

(require 'project)
(require 'cl-lib)

(defgroup android-mode nil
  "A minor mode for Android application development."
  :prefix "android-mode-"
  :group 'applications)

(defcustom android-mode-avd ""
  "Default AVD to use."
  :type 'string
  :group 'android-mode)

(defcustom android-mode-sdk-dir nil
  "Set to the directory containing the Android SDK.
This value will be overridden by ANDROID_HOME environment variable when
available."
  :type 'string
  :group 'android-mode)

(defcustom android-mode-sdk-tool-subdirs '("emulator" "tools" "platform-tools")
  "List of subdirectories in the SDK containing commandline tools."
  :type '(repeat string)
  :group 'android-mode)

(defcustom android-mode-sdk-tool-extensions '("" ".bat" ".exe")
  "List of possible extensions for commandline tools."
  :type '(repeat string)
  :group 'android-mode)

(defvar android-mode-flavor-script
  (expand-file-name "listFlavorAppId.gradle"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Gradle init script path relative to this Emacs Lisp file.")

(defun android-root ()
  "Find the root directory of the Android project.
The root is the directory containing the project's `gradlew` file."
  (locate-dominating-file default-directory "gradlew"))

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

(defun android-list-avd ()
  "List of Android Virtual Devices installed on local machine.
Uses the modern `emulator -list-avds` command."
  (let* ((command (format "%s -list-avds" (android-tool-path "emulator")))
         (output (shell-command-to-string command))
         (result (split-string output "\n" t)))
    (if result
        (nreverse result)
      (error "No Android Virtual Devices found"))))

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

(defun android--find-module-dir (dir)
  "Return a list of subdirectories of DIR that contain a
`build/outputs/apk` directory."
  (when-let ((dir (file-name-as-directory dir)))
    (seq-filter
     (lambda (sub)
       (and (file-directory-p sub) (file-directory-p (concat sub "build/outputs/apk"))))
     (directory-files dir t "^[^.]" t))))

(defun android--apk-path ()
  "Find the most recent debug APK in the project build output."
  (android-in-directory
   (android-root)
   (let* ((modules (or (android--find-module-dir default-directory)
                       (list default-directory)))
          (candidates (mapcan
                       (lambda (mod)
                         (let ((apk-dir (concat mod "build/outputs/apk/debug/")))
                           (when (file-directory-p apk-dir)
                             (directory-files apk-dir t "\\.apk$"))))
                       modules)))
     (when candidates
       (car (sort candidates
                  (lambda (a b)
                    (time-less-p (file-attribute-modification-time (file-attributes b))
                                 (file-attribute-modification-time (file-attributes a))))))))))

(defun android--aapt2-dump (apk)
  "Run `aapt2 dump badging APK` and return the output."
  (shell-command-to-string
   (format "%s dump badging %s 2>&1"
           (android-tool-path "aapt2")
           (shell-quote-argument apk))))

(defun android-project-package ()
  "Return the package name of the Android project.
Parses the built APK via aapt2."
  (when-let ((apk (android--apk-path)))
    (let ((output (android--aapt2-dump apk)))
      (when (string-match "^package: name='\\([^']+\\)'" output)
        (match-string 1 output)))))

(defun android-project-main-activities (&optional _category)
  "Return list of main activity class names.
Parses the built APK via aapt2."
  (when-let ((apk (android--apk-path)))
    (let ((output (android--aapt2-dump apk))
          activities)
      (with-temp-buffer
        (insert output)
        (goto-char (point-min))
        (while (re-search-forward "^launchable-activity: name='\\([^']+\\)'" nil t)
          (push (match-string 1) activities)))
      (nreverse activities))))

(defun android-start-app ()
  "Start the main activity in the running emulator.
Tries to match the current buffer's class name against activities
parsed from the built APK.  Falls back to the first launchable activity."
  (interactive)
  (let* ((apk (android--apk-path))
         (dump (when apk (android--aapt2-dump apk)))
         package launchable)
    (unless apk (error "No APK found. Run `./gradlew assembleDebug` first."))
    (when (string-match "^package: name='\\([^']+\\)'" dump)
      (setq package (match-string 1 package)))
    (let ((pos 0))
      (while (string-match "launchable-activity: name='\\([^']+\\)'" dump pos)
        (push (match-string 1 dump) launchable)
        (setq pos (match-end 0))))
    (setq launchable (nreverse launchable))
    (let* ((current (android-current-buffer-class-name))
           (activity (or (and current (seq-contains-p launchable current #'string=)) (car launchable))))
      (unless activity (error "No launchable activity found in APK"))
      (message "Starting activity: %s" activity)
      (let* ((command (format "%s shell am start -n %s/%s"
                              (android-tool-path "adb") package activity))
             (output (shell-command-to-string command)))
        (when (string-match-p "^Error: " output)
          (error "Error starting app:\n%s" output))))))

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

(defun android-gradle (tasks-or-goals)
  "Run gradle TASKS-OR-GOALS in the project root directory."
  (interactive "sTasks or Goals: ")
  (android-in-directory
   (android-root)
   (compile (format "./gradlew %s" tasks-or-goals))))

;; Gradle
(defmacro android-defun-gradle-task (task)
  `(defun ,(intern (concat "android-gradle-"
                           (replace-regexp-in-string "[[:space:]:]" "-" task)))
       ()
     ,(concat "Run `gradle " task "` in the project root directory.")
     (interactive)
     (android-gradle ,task)))

(android-defun-gradle-task "clean")
(android-defun-gradle-task "test")
(android-defun-gradle-task "assembleDebug")
(android-defun-gradle-task "assembleRelease")
(android-defun-gradle-task "installDebug")
(android-defun-gradle-task "uninstallDebug")

(defconst android-mode-keys
  '(("a" . android-start-app)
    ("e" . android-start-emulator)
    ("C" . android-gradle-clean)
    ("t" . android-gradle-test)
    ("c" . android-gradle-assembleDebug)
    ("i" . android-gradle-installDebug)
    ("u" . android-gradle-uninstallDebug)))

(defvar android-mode-map (make-sparse-keymap)
  "Keymap for `android-mode'.")

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

(defun android--latest-build-tools-subdir ()
  "Return the relative path to the latest build-tools subdirectory."
  (let* ((build-tools-dir (expand-file-name "build-tools" (android-local-sdk-dir)))
         (versions (and (file-directory-p build-tools-dir)
                        (directory-files build-tools-dir nil "^[0-9]"))))
    (when versions
      (concat "build-tools/" (car (last (sort versions #'string<)))))))

(when-let ((subdir (ignore-errors (android--latest-build-tools-subdir))))
  (cl-pushnew subdir android-mode-sdk-tool-subdirs :test #'string=))

(provide 'android-mode)

;;; android-mode.el ends here
