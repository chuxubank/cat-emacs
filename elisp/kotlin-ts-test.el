;;; kotlin-ts-test.el --- Run Kotlin tests from tree-sitter buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Misaka

;; Author: Misaka <chuxubank@qq.com>
;; Maintainer: Misaka <chuxubank@qq.com>
;; Version: 0.1.0
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
;; Also provides `kotlin-ts-test-find-sibling-rules' for
;; `find-sibling-file' integration, installed via a mode hook.

;;; Code:

(require 'kotlin-ts-mode)

;; ──────────────────────────────────────────────────────────────────
;;; Source-set detection
;; ──────────────────────────────────────────────────────────────────

(defconst kotlin-ts-test--source-set-re
  (rx "src/"
      (group (group (+? anything)) (or "Main" "Test"))
      "/kotlin/")
  "Regexp matching a Kotlin source set directory.
Group 1: full source-set name (e.g. \"commonMain\", \"jvmTest\").
Group 2: the target prefix  (e.g. \"common\", \"jvm\").
An empty group 2 means the standard JVM layout (src/main, src/test).")

(defconst kotlin-ts-test--standard-re
  (rx "src/" (group (or "main" "test")) "/kotlin/")
  "Regexp matching the standard JVM layout (src/main/kotlin, src/test/kotlin).")

(defun kotlin-ts-test--detect-layout ()
  "Detect the project layout from the current buffer's file path.
Return a plist (:layout kmp :target \"common\") or
\(:layout jvm :target nil) or nil if nothing matches."
  (when-let* ((file (buffer-file-name)))
    (cond
     ;; KMP: src/<target>Main/kotlin or src/<target>Test/kotlin
     ((string-match kotlin-ts-test--source-set-re file)
      (let ((target (match-string 2 file)))
        (unless (string-empty-p target)
          (list :layout 'kmp :target target))))
     ;; Standard JVM: src/main/kotlin or src/test/kotlin
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
  :group 'kotlin
  :safe #'listp)

(defcustom kotlin-ts-test-aggregate-tasks '("allTests")
  "List of Gradle task names that are aggregate (lifecycle) tasks.
Aggregate tasks do not support the `--tests' filter flag and will
be run without it.  Non-aggregate tasks receive `--tests' to
select specific classes or functions."
  :type '(repeat string)
  :group 'kotlin
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
           ;; Figure out whether we are in a Main/source or Test file
           (in-test (string-match-p
                     (if (eq layout 'kmp)
                         (concat "src/" target "Test/")
                       "src/test/")
                     file))
           ;; Build from/to set names
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
;;; Run tests
;; ──────────────────────────────────────────────────────────────────

;;;###autoload
(defun kotlin-ts-test-run-class ()
  "Run the test class at point via Gradle.
Automatically selects the correct task for JVM or KMP layouts.
If the resolved task is an aggregate task (see
`kotlin-ts-test-aggregate-tasks'), run it without `--tests'."
  (interactive)
  (let* ((info (kotlin-ts-test--detect-layout))
         (task (kotlin-ts-test--gradle-task info)))
    (if (kotlin-ts-test--aggregate-task-p task)
        (kotlin-ts-mode--run-gradle-command
         (kotlin-ts-mode--get-subproject-name) task nil)
      (let ((package-name (kotlin-ts-mode--get-package-name))
            (class-name   (kotlin-ts-mode--get-class-name)))
        (unless (and package-name class-name)
          (user-error "Could not determine package and class name"))
        (kotlin-ts-mode--run-gradle-command
         (kotlin-ts-mode--get-subproject-name)
         task
         (list "--tests"
               (kotlin-ts-mode--qualify-name package-name class-name)))))))

;;;###autoload
(defun kotlin-ts-test-run-function ()
  "Run the test function at point via Gradle.
Automatically selects the correct task for JVM or KMP layouts.
If the resolved task is an aggregate task (see
`kotlin-ts-test-aggregate-tasks'), run it without `--tests'."
  (interactive)
  (let* ((info (kotlin-ts-test--detect-layout))
         (task (kotlin-ts-test--gradle-task info)))
    (if (kotlin-ts-test--aggregate-task-p task)
        (kotlin-ts-mode--run-gradle-command
         (kotlin-ts-mode--get-subproject-name) task nil)
      (let ((package-name  (kotlin-ts-mode--get-package-name))
            (class-name    (kotlin-ts-mode--get-class-name))
            (function-name (kotlin-ts-mode--get-function-name)))
        (unless (and package-name class-name function-name)
          (user-error "Could not determine package, class, and function name"))
        (kotlin-ts-mode--run-gradle-command
         (kotlin-ts-mode--get-subproject-name)
         task
         (list "--tests"
               (kotlin-ts-mode--qualify-name
                package-name class-name function-name)))))))

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
   ;; Standard JVM (from upstream)
   kotlin-ts-mode--find-sibling-rules
   ;; KMP: <target>Main ↔ <target>Test
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
