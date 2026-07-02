;; -*- lexical-binding: t; -*-

(defconst cat-core-directory (file-name-directory load-file-name)
  "Directory containing Cat Emacs core files.")

(defun cat-load-file (file context &optional noerror)
  "Load FILE for CONTEXT with Cat benchmark and error reporting."
  (condition-case-unless-debug err
      (let (file-name-handler-alist)
        (cat-benchmark 'beg file)
        (load file noerror 'nomessage))
    (error
     (message "ERROR: %S when loading %s: %s\nBacktrace:\n%s"
              err
              context
              (abbreviate-file-name file)
              (with-output-to-string (backtrace))))))

(defvar cat-core-modules '(package utils)
  "Core files loaded before optional Cat modules.")

(defun cat-load-core-file (module &optional noerror)
  "Load core MODULE under the core directory."
  (cat-load-file
   (expand-file-name (symbol-name module) cat-core-directory)
   "core file"
   noerror))

(defun cat-load-core ()
  "Load Cat Emacs core files."
  (dolist (module cat-core-modules)
    (cat-load-core-file module)))

(cat-load-core)
(cat-load-file (expand-file-name "module" cat-core-directory) "core module loader")

(provide 'cat-core)
