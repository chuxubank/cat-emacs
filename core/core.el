;; -*- lexical-binding: t; -*-

(require 'seq)

(defconst cat-core-directory (file-name-directory load-file-name)
  "Directory containing Cat Emacs core files.")

(defun cat-core-feature (file)
  "Return the core feature provided by FILE."
  (intern (concat "cat-" (file-name-base file))))

(defun cat-core-files ()
  "Return Cat core files, excluding the core entry file itself."
  (sort
   (seq-remove
    (lambda (file)
      (string= (file-name-nondirectory file) "core.el"))
    (directory-files cat-core-directory t "\\.el\\'"))
   #'string<))

(defun cat-require-core-file (file)
  "Require the core feature provided by FILE."
  (let ((feature (cat-core-feature file)))
    (unless (featurep feature)
      (condition-case-unless-debug err
          (progn
            (cat-benchmark 'beg file)
            (require feature file))
        (error
         (message "ERROR: %S when loading core feature: %s\nBacktrace:\n%s"
                  err
                  (abbreviate-file-name file)
                  (with-output-to-string (backtrace))))))))

(defun cat-load-core ()
  "Load Cat Emacs core files."
  (dolist (file (cat-core-files))
    (cat-require-core-file file)))

(cat-load-core)

(unless IS-CI
  (message "%s not running on CI, load modules" cat-emacs-name)
  (cat-load-modules))

(provide 'cat-core)
