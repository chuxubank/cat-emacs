;;; kotlin-ts-test.el --- Run Kotlin tests from tree-sitter buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Misaka

;; Author: Misaka <chuxubank@qq.com>
;; Maintainer: Misaka <chuxubank@qq.com>
;; Version: 0.3.0
;; Package-Requires: ((emacs "30.1") (kotlin-ts-mode))
;; Keywords: languages, kotlin, tools
;; URL: https://github.com/chuxubank/cat-emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A self-contained test-runner for `kotlin-ts-mode' that handles both
;; standard Kotlin/JVM and Kotlin Multiplatform (KMP) project layouts.
;;
;; `kotlin-ts-mode' ships with test helpers that hardcode the JVM
;; convention (src/main/kotlin, src/test/kotlin, Gradle `:test' task).
;; This package replaces those with layout-aware versions:
;;
;;   Standard JVM          KMP
;;   ──────────────        ──────────────────────
;;   src/main/kotlin   →   src/commonMain/kotlin
;;   src/test/kotlin   →   src/commonTest/kotlin
;;   :test             →   :allTests / :jvmTest / …
;;
;; Three interactive commands:
;;   `kotlin-ts-test-goto-file'          – jump to / from test file
;;   `kotlin-ts-test-run-class'          – run test class at point
;;   `kotlin-ts-test-run-function'       – run test function at point
;;
;; When invoked from a source file (non-test), `run-class' and
;; `run-function' automatically derive the corresponding test class
;; by appending "Test" to the current class name (e.g. Foo → FooTest).
;;
;; After a test run finishes, the JUnit XML reports under
;; build/test-results/<task>/ are parsed and displayed in a
;; *kotlin-ts-test-results* buffer with a summary.
;;
;; Also provides `kotlin-ts-test-find-sibling-rules' for
;; `find-sibling-file' integration, installed via a mode hook.

;;; Code:

(require 'kotlin-ts-mode)
(require 'compile)
(require 'xml)
(require 'dom)
(require 'seq)

;; ──────────────────────────────────────────────────────────────────
;;; Custom group
;; ──────────────────────────────────────────────────────────────────

(defgroup kotlin-ts-test nil
  "Run Kotlin tests from `kotlin-ts-mode' buffers."
  :group 'kotlin
  :prefix "kotlin-ts-test-")

;; ──────────────────────────────────────────────────────────────────
;;; Source-set detection
;; ──────────────────────────────────────────────────────────────────

(defconst kotlin-ts-test--source-set-re
  (rx "src/"
      (group (group (+? anything)) (or "Main" "Test"))
      "/kotlin/")
  "Regexp matching a Kotlin source set directory.
Group 1: full source-set name (e.g. \"commonMain\", \"jvmTest\").
Group 2: the target prefix  (e.g. \"common\", \"jvm\").")

(defconst kotlin-ts-test--standard-re
  (rx "src/" (group (or "main" "test")) "/kotlin/")
  "Regexp matching the standard JVM layout (src/main/kotlin, src/test/kotlin).")

(defun kotlin-ts-test--detect-layout ()
  "Detect the project layout from the current buffer's file path.
Return a plist (:layout kmp :target \"common\") or
\(:layout jvm :target nil) or nil if nothing matches."
  (when-let* ((file (buffer-file-name)))
    (cond
     ((string-match kotlin-ts-test--source-set-re file)
      (let ((target (match-string 2 file)))
        (unless (string-empty-p target)
          (list :layout 'kmp :target target))))
     ((string-match kotlin-ts-test--standard-re file)
      (list :layout 'jvm :target nil)))))

;; ──────────────────────────────────────────────────────────────────
;;; Gradle task mapping
;; ──────────────────────────────────────────────────────────────────

(defcustom kotlin-ts-test-task-alist nil
  "Alist mapping target names to Gradle test task names.
Each entry is (TARGET . TASK) where TARGET is the KMP target
string and TASK is the Gradle task to run.

Entries here take precedence over the built-in defaults.
For example, to run \"desktopTest\" instead of \"allTests\" for
commonTest sources:

  \\='((\"common\" . \"desktopTest\"))

Or per-project via .dir-locals.el:

  ((kotlin-ts-mode
    (kotlin-ts-test-task-alist
     (\"common\" . \"desktopTest\")
     (\"jvm\" . \"jvmTest\"))))"
  :type '(alist :key-type string :value-type string)
  :group 'kotlin-ts-test
  :safe #'listp)

(defcustom kotlin-ts-test-aggregate-tasks '("allTests")
  "List of Gradle task names that are aggregate (lifecycle) tasks.
Aggregate tasks do not support the `--tests' filter flag and will
be run without it.  Non-aggregate tasks receive `--tests' to
select specific classes or functions."
  :type '(repeat string)
  :group 'kotlin-ts-test
  :safe #'listp)

(defun kotlin-ts-test--gradle-task (layout-info)
  "Return the Gradle test task name for LAYOUT-INFO.
LAYOUT-INFO is the plist from `kotlin-ts-test--detect-layout'.

Consult `kotlin-ts-test-task-alist' first; fall back to built-in
defaults:
  JVM / nil        → \"test\"
  KMP \"common\"   → \"allTests\"
  KMP \"jvm\"      → \"jvmTest\"  (etc.)"
  (let ((target (plist-get layout-info :target)))
    (or (alist-get target kotlin-ts-test-task-alist nil nil #'equal)
        (pcase target
          ((or 'nil "") "test")
          ("common" "allTests")
          (other (concat other "Test"))))))

(defun kotlin-ts-test--aggregate-task-p (task)
  "Return non-nil if TASK is an aggregate task that rejects `--tests'."
  (member task kotlin-ts-test-aggregate-tasks))

;; ──────────────────────────────────────────────────────────────────
;;; Test name mapping
;; ──────────────────────────────────────────────────────────────────

(defcustom kotlin-ts-test-function-pattern "*%s*"
  "Format string for deriving a test function filter from a source function name.
Used when running tests from a source (non-test) file.

`%s' is replaced by the source function name.  The resulting
string is passed to Gradle's `--tests' flag, which supports `*'
as a wildcard.

Examples:
  \"*%s*\"     →  matches any test containing the function name (default)
  \"test%s\"   →  matches testFoo for source function foo
  \"%s\"       →  exact match only

This can also be set per-project via .dir-locals.el."
  :type 'string
  :group 'kotlin-ts-test
  :safe #'stringp)

;; ──────────────────────────────────────────────────────────────────
;;; Test file predicate
;; ──────────────────────────────────────────────────────────────────

(defun kotlin-ts-test--in-test-p ()
  "Return non-nil if the current buffer is a test file."
  (when-let* ((file (buffer-file-name)))
    (or (string-match-p (rx "src/" (+? anything) "Test/kotlin/") file)
        (string-match-p (rx "src/test/kotlin/") file))))

;; ──────────────────────────────────────────────────────────────────
;;; Goto test / source file
;; ──────────────────────────────────────────────────────────────────

(defun kotlin-ts-test--swap-path (file from to)
  "In FILE, replace the FROM source-set component with TO."
  (string-replace (concat "src/" from "/kotlin/")
                  (concat "src/" to   "/kotlin/")
                  file))

(defun kotlin-ts-test--toggle-test-suffix (base is-test)
  "Return the counterpart file base name.
If IS-TEST, strip the trailing \"Test\"; otherwise append it."
  (if is-test
      (string-remove-suffix "Test" base)
    (concat base "Test")))

;;;###autoload
(defun kotlin-ts-test-goto-file ()
  "Toggle between source and test file.

Supports:
  src/main/kotlin/…/Foo.kt       ↔  src/test/kotlin/…/FooTest.kt
  src/<target>Main/kotlin/…/Foo.kt ↔  src/<target>Test/kotlin/…/FooTest.kt"
  (interactive)
  (let* ((file (buffer-file-name))
         (info (kotlin-ts-test--detect-layout)))
    (unless info
      (user-error "Cannot determine Kotlin source layout for %s" file))
    (let* ((layout (plist-get info :layout))
           (target (plist-get info :target))
           (in-test (string-match-p
                     (if (eq layout 'kmp)
                         (concat "src/" target "Test/")
                       "src/test/")
                     file))
           (from-set (if (eq layout 'kmp)
                         (concat target (if in-test "Test" "Main"))
                       (if in-test "test" "main")))
           (to-set   (if (eq layout 'kmp)
                         (concat target (if in-test "Main" "Test"))
                       (if in-test "main" "test")))
           (swapped (kotlin-ts-test--swap-path file from-set to-set))
           (dest-dir (file-name-directory swapped))
           (base (file-name-base file))
           (dest-base (kotlin-ts-test--toggle-test-suffix base in-test))
           (dest (concat dest-dir dest-base ".kt")))
      (find-file dest))))

;; ──────────────────────────────────────────────────────────────────
;;; Gradle runner
;; ──────────────────────────────────────────────────────────────────

(defconst kotlin-ts-test--compilation-buffer-name "*kotlin-ts-test*"
  "Name of the compilation buffer used for test runs.")

(defun kotlin-ts-test--compilation-buffer-name-fn (_mode)
  "Return the fixed compilation buffer name."
  kotlin-ts-test--compilation-buffer-name)

(defvar kotlin-ts-test--last-command nil
  "The last Gradle command string, for rerunning.")

(defvar kotlin-ts-test--last-run nil
  "Plist describing the last test run.
Keys: :project-root :subproject :task.")

(defun kotlin-ts-test--run-gradle (project task args)
  "Run Gradle TASK with ARGS in PROJECT, parse results when done.
If PROJECT is nil, run in the root project."
  (let* ((default-directory default-directory)
         (exec-path exec-path)
         (command "gradle")
         (compilation-buffer-name-function
          #'kotlin-ts-test--compilation-buffer-name-fn)
         (qualified-task (if project
                             (concat ":" project ":" task)
                           (concat ":" task))))
    (when (kotlin-ts-mode--in-gradle-project-p)
      (setq default-directory (project-root (project-current))
            command "./gradlew"
            exec-path (list nil)))
    (let ((cmd (string-join
                (append
                 (list command "--console=plain" qualified-task)
                 (when args
                   (mapcar #'shell-quote-argument args)))
                " ")))
      ;; Remember for rerun and for finding XML reports
      (setq kotlin-ts-test--last-command cmd
            kotlin-ts-test--last-run (list :project-root default-directory
                                          :subproject project
                                          :task task))
      (compile cmd)
      (when-let* ((buf (get-buffer kotlin-ts-test--compilation-buffer-name)))
        (with-current-buffer buf
          (add-hook 'compilation-finish-functions
                    #'kotlin-ts-test--on-compilation-finish nil t))))))

;; ──────────────────────────────────────────────────────────────────
;;; JUnit XML parsing
;; ──────────────────────────────────────────────────────────────────

;; Gradle writes JUnit XML reports to:
;;   <project-root>/<subproject>/build/test-results/<task>/
;; Each file is TEST-<classname>.xml containing a <testsuite> with
;; <testcase> children.  Failed tests have a <failure> child,
;; skipped tests have a <skipped> child.

(cl-defstruct (kotlin-ts-test-result (:constructor kotlin-ts-test-result-create)
                                     (:copier nil))
  "A single test result."
  class name status time message)

(defun kotlin-ts-test--report-dir ()
  "Return the directory containing JUnit XML reports for the last run."
  (when-let* ((run kotlin-ts-test--last-run)
              (root (plist-get run :project-root))
              (task (plist-get run :task)))
    (let* ((subproject (plist-get run :subproject))
           (base (if subproject
                     (concat root
                             (string-replace ":" "/" subproject)
                             "/")
                   root)))
      (expand-file-name (concat "build/test-results/" task "/") base))))

(defun kotlin-ts-test--parse-xml-file (file)
  "Parse a single JUnit XML FILE and return a list of `kotlin-ts-test-result'."
  (condition-case nil
      (let* ((root (car (xml-parse-file file)))
             (testcases (dom-by-tag root 'testcase)))
        (mapcar
         (lambda (tc)
           (let* ((classname (dom-attr tc 'classname))
                  (name      (dom-attr tc 'name))
                  (time-str  (dom-attr tc 'time))
                  (failure   (dom-by-tag tc 'failure))
                  (error     (dom-by-tag tc 'error))
                  (skipped   (dom-by-tag tc 'skipped))
                  (status    (cond (failure "FAILED")
                                  (error   "FAILED")
                                  (skipped "SKIPPED")
                                  (t       "PASSED")))
                  (message   (cond (failure (dom-attr (car failure) 'message))
                                  (error   (dom-attr (car error) 'message))
                                  (t       nil))))
             (kotlin-ts-test-result-create
              :class   classname
              :name    name
              :status  status
              :time    (and time-str (string-to-number time-str))
              :message message)))
         testcases))
    (error nil)))

(defun kotlin-ts-test--parse-reports ()
  "Parse all JUnit XML reports for the last test run.
Return a list of `kotlin-ts-test-result'."
  (when-let* ((dir (kotlin-ts-test--report-dir))
              (_exists (file-directory-p dir)))
    (let ((xml-files (directory-files dir t (rx ".xml" eos))))
      (mapcan #'kotlin-ts-test--parse-xml-file xml-files))))

;; ──────────────────────────────────────────────────────────────────
;;; Results buffer
;; ──────────────────────────────────────────────────────────────────

(defconst kotlin-ts-test-results-buffer-name "*kotlin-ts-test-results*"
  "Name of the test results display buffer.")

(defface kotlin-ts-test-pass-face
  '((t :foreground "green" :weight bold))
  "Face for PASSED tests."
  :group 'kotlin-ts-test)

(defface kotlin-ts-test-fail-face
  '((t :foreground "red" :weight bold))
  "Face for FAILED tests."
  :group 'kotlin-ts-test)

(defface kotlin-ts-test-skip-face
  '((t :foreground "yellow" :weight bold))
  "Face for SKIPPED tests."
  :group 'kotlin-ts-test)

(defface kotlin-ts-test-class-face
  '((t :inherit font-lock-type-face))
  "Face for test class names in the results buffer."
  :group 'kotlin-ts-test)

(defface kotlin-ts-test-name-face
  '((t :inherit font-lock-function-name-face))
  "Face for test function names in the results buffer."
  :group 'kotlin-ts-test)

(defvar kotlin-ts-test-results-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'quit-window)
    (define-key map "g" #'kotlin-ts-test-rerun)
    map)
  "Keymap for `kotlin-ts-test-results-mode'.")

(define-derived-mode kotlin-ts-test-results-mode special-mode "KtTest"
  "Major mode for displaying Kotlin test results."
  :group 'kotlin-ts-test
  (setq truncate-lines t))

(defun kotlin-ts-test--status-face (status)
  "Return the face for STATUS string."
  (pcase status
    ("PASSED"  'kotlin-ts-test-pass-face)
    ("FAILED"  'kotlin-ts-test-fail-face)
    ("SKIPPED" 'kotlin-ts-test-skip-face)
    (_         'default)))

(defun kotlin-ts-test--status-icon (status)
  "Return a display icon for STATUS string."
  (pcase status
    ("PASSED"  "✓")
    ("FAILED"  "✗")
    ("SKIPPED" "○")
    (_         "?")))

(defun kotlin-ts-test--render-results (results compilation-buf)
  "Render RESULTS into the results buffer.
COMPILATION-BUF is the source compilation buffer for linking."
  (let* ((buf (get-buffer-create kotlin-ts-test-results-buffer-name))
         (passed  (cl-count "PASSED"  results :key #'kotlin-ts-test-result-status :test #'equal))
         (failed  (cl-count "FAILED"  results :key #'kotlin-ts-test-result-status :test #'equal))
         (skipped (cl-count "SKIPPED" results :key #'kotlin-ts-test-result-status :test #'equal))
         (total   (length results))
         (total-time (apply #'+ (mapcar (lambda (r) (or (kotlin-ts-test-result-time r) 0)) results)))
         ;; Group by class
         (groups  (seq-group-by #'kotlin-ts-test-result-class results)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (kotlin-ts-test-results-mode)
        ;; Header
        (insert (propertize "Kotlin Test Results" 'face '(:weight bold :height 1.2)))
        (insert (format "  (%.3fs)\n" total-time))
        (insert (make-string 50 ?─) "\n\n")
        ;; Summary line
        (insert (format "  Total: %d" total))
        (when (> passed 0)
          (insert "  " (propertize (format "✓ %d passed" passed) 'face 'kotlin-ts-test-pass-face)))
        (when (> failed 0)
          (insert "  " (propertize (format "✗ %d failed" failed) 'face 'kotlin-ts-test-fail-face)))
        (when (> skipped 0)
          (insert "  " (propertize (format "○ %d skipped" skipped) 'face 'kotlin-ts-test-skip-face)))
        (insert "\n\n")
        ;; Per-class results (failed classes first)
        (let ((sorted-groups
               (seq-sort-by (lambda (g)
                              (if (cl-some (lambda (r) (equal (kotlin-ts-test-result-status r) "FAILED"))
                                           (cdr g))
                                  0 1))
                            #'< groups)))
          (dolist (group sorted-groups)
            (let ((class (car group))
                  (tests (cdr group)))
              (insert (propertize (or class "?") 'face 'kotlin-ts-test-class-face) "\n")
              (dolist (test tests)
                (let* ((status (kotlin-ts-test-result-status test))
                       (icon   (kotlin-ts-test--status-icon status))
                       (face   (kotlin-ts-test--status-face status))
                       (time   (kotlin-ts-test-result-time test))
                       (msg    (kotlin-ts-test-result-message test)))
                  (insert "  "
                          (propertize icon 'face face)
                          " "
                          (propertize (kotlin-ts-test-result-name test) 'face 'kotlin-ts-test-name-face))
                  (when time
                    (insert (propertize (format " (%.3fs)" time) 'face 'shadow)))
                  (insert "\n")
                  (when (and msg (equal status "FAILED"))
                    (insert "    " (propertize msg 'face 'kotlin-ts-test-fail-face) "\n"))))
              (insert "\n"))))
        ;; Footer
        (insert (make-string 50 ?─) "\n")
        (insert (propertize "q" 'face 'help-key-binding) " quit  "
                (propertize "g" 'face 'help-key-binding) " rerun  ")
        (when compilation-buf
          (insert "Full output: "
                  (propertize (buffer-name compilation-buf) 'face 'link)))
        (insert "\n")
        (goto-char (point-min))))
    ;; Display the buffer
    (display-buffer buf '((display-buffer-reuse-window
                           display-buffer-at-bottom)
                          (window-height . fit-window-to-buffer)))))

;; ──────────────────────────────────────────────────────────────────
;;; Compilation finish hook
;; ──────────────────────────────────────────────────────────────────

(defun kotlin-ts-test--on-compilation-finish (buf _msg)
  "Parse JUnit XML test reports and display results after compilation.
BUF is the compilation buffer."
  (let ((results (kotlin-ts-test--parse-reports)))
    (if results
        (kotlin-ts-test--render-results results buf)
      (message "kotlin-ts-test: no test results found in %s"
               (or (kotlin-ts-test--report-dir) "unknown")))))

;; ──────────────────────────────────────────────────────────────────
;;; Run tests
;; ──────────────────────────────────────────────────────────────────

;;;###autoload
(defun kotlin-ts-test-run-class ()
  "Run the corresponding test class via Gradle.
When in a test file, run the test class at point.
When in a source file, derive the test class name by appending
\"Test\" to the current class name and run that instead.
If the resolved task is an aggregate task (see
`kotlin-ts-test-aggregate-tasks'), run it without `--tests'."
  (interactive)
  (let* ((info (kotlin-ts-test--detect-layout))
         (task (kotlin-ts-test--gradle-task info)))
    (if (kotlin-ts-test--aggregate-task-p task)
        (kotlin-ts-test--run-gradle
         (kotlin-ts-mode--get-subproject-name) task nil)
      (let* ((package-name (kotlin-ts-mode--get-package-name))
             (class-name   (kotlin-ts-mode--get-class-name))
             (test-class   (if (kotlin-ts-test--in-test-p)
                               class-name
                             (when class-name
                               (concat class-name "Test")))))
        (unless (and package-name test-class)
          (user-error "Could not determine package and class name"))
        (kotlin-ts-test--run-gradle
         (kotlin-ts-mode--get-subproject-name)
         task
         (list "--tests"
               (kotlin-ts-mode--qualify-name package-name test-class)))))))

;;;###autoload
(defun kotlin-ts-test-run-function ()
  "Run the corresponding test function via Gradle.
When in a test file, run the test function at point exactly.
When in a source file, derive the test class name by appending
\"Test\" to the current class name and use
`kotlin-ts-test-function-pattern' to build a wildcard filter
from the source function name (default \"*funcName*\").
If the resolved task is an aggregate task (see
`kotlin-ts-test-aggregate-tasks'), run it without `--tests'."
  (interactive)
  (let* ((info (kotlin-ts-test--detect-layout))
         (task (kotlin-ts-test--gradle-task info)))
    (if (kotlin-ts-test--aggregate-task-p task)
        (kotlin-ts-test--run-gradle
         (kotlin-ts-mode--get-subproject-name) task nil)
      (let* ((package-name  (kotlin-ts-mode--get-package-name))
             (class-name    (kotlin-ts-mode--get-class-name))
             (function-name (kotlin-ts-mode--get-function-name))
             (in-test       (kotlin-ts-test--in-test-p))
             (test-class    (if in-test
                                class-name
                              (when class-name
                                (concat class-name "Test"))))
             (test-function (if in-test
                                function-name
                              (when function-name
                                (format kotlin-ts-test-function-pattern
                                        function-name)))))
        (unless (and package-name test-class test-function)
          (user-error "Could not determine package, class, and function name"))
        (kotlin-ts-test--run-gradle
         (kotlin-ts-mode--get-subproject-name)
         task
         (list "--tests"
               (kotlin-ts-mode--qualify-name
                package-name test-class test-function)))))))

;;;###autoload
(defun kotlin-ts-test-rerun ()
  "Rerun the last test command."
  (interactive)
  (if-let* ((cmd kotlin-ts-test--last-command)
            (run kotlin-ts-test--last-run))
      (let ((compilation-buffer-name-function
             #'kotlin-ts-test--compilation-buffer-name-fn)
            (default-directory (plist-get run :project-root)))
        (compile cmd)
        (when-let* ((buf (get-buffer kotlin-ts-test--compilation-buffer-name)))
          (with-current-buffer buf
            (add-hook 'compilation-finish-functions
                      #'kotlin-ts-test--on-compilation-finish nil t))))
    (user-error "No previous test run to repeat")))

;; ──────────────────────────────────────────────────────────────────
;;; find-sibling-file rules
;; ──────────────────────────────────────────────────────────────────

(defconst kotlin-ts-test--kmp-targets
  '("common" "jvm" "android" "js" "wasmJs" "wasmWasi"
    "native" "ios" "iosArm64" "iosSimulatorArm64" "iosX64"
    "macos" "macosArm64" "macosX64"
    "linux" "linuxX64" "linuxArm64"
    "mingw" "mingwX64"
    "tvos" "tvosArm64" "tvosSimulatorArm64" "tvosX64"
    "watchos" "watchosArm32" "watchosArm64"
    "watchosSimulatorArm64" "watchosX64")
  "Known KMP target names used to generate `find-sibling-rules'.")

(defvar kotlin-ts-test-find-sibling-rules
  (append
   kotlin-ts-mode--find-sibling-rules
   (mapcan
    (lambda (tgt)
      (let ((main (concat tgt "Main/"))
            (test (concat tgt "Test/")))
        (list
         (list (concat (regexp-quote main) "\\(.+/\\)\\([^/]+\\)\\.kt")
               (concat test "\\1\\2Test.kt"))
         (list (concat (regexp-quote test) "\\(.+/\\)\\([^/]+\\)Test\\.kt")
               (concat main "\\1\\2.kt")))))
    kotlin-ts-test--kmp-targets))
  "Sibling rules for `find-sibling-file', covering JVM and KMP layouts.")

;; ──────────────────────────────────────────────────────────────────
;;; Mode hook
;; ──────────────────────────────────────────────────────────────────

(defun kotlin-ts-test--setup ()
  "Install KMP-aware sibling rules in the current buffer."
  (setq-local find-sibling-rules kotlin-ts-test-find-sibling-rules))

;;;###autoload
(add-hook 'kotlin-ts-mode-hook #'kotlin-ts-test--setup)

(provide 'kotlin-ts-test)
;;; kotlin-ts-test.el ends here
