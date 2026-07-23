;; -*- lexical-binding: t; -*-

(defconst cat-core-directory (file-name-directory load-file-name)
  "Directory containing Cat Emacs core files.")

(defconst cat-core--libraries
  '((cat-config . "config")
    (cat-module . "module")
    ;; Package management depends on configuration and module queries.
    (cat-package-archives . "package/archives")
    (cat-package-manifest . "package/manifest")
    (cat-package-use-package . "package/use-package")
    (cat-package-vc-skip-unchanged . "package/vc-skip-unchanged")
    (cat-utils . "utils"))
  "Core libraries in dependency order.")

(defvar cat-core-state 'new
  "Current core initialization state.
The value is one of `new', `initializing', `ready', or `failed'.")

(defun cat-core--require (feature file)
  "Load FEATURE from core FILE unless it is already available."
  (unless (featurep feature)
    (let ((path (expand-file-name file cat-core-directory)))
      (cat-benchmark 'beg path)
      (require feature path))))

(defun cat-core-initialize ()
  "Load core libraries and the configured Cat modules exactly once."
  (cond
   ((eq cat-core-state 'ready)
    cat-core-state)
   ((eq cat-core-state 'initializing)
    (error "Recursive Cat core initialization"))
   (t
    (setq cat-core-state 'initializing)
    (let (succeeded)
      (unwind-protect
        (progn
          (dolist (library cat-core--libraries)
            (cat-core--require (car library) (cdr library)))
          (unless IS-CI
            (message "%s not running on CI, load modules" cat-emacs-name)
            (cat-package-collect #'cat-load-modules))
          (setq cat-core-state 'ready
                succeeded t))
        (unless succeeded
          (setq cat-core-state 'failed)))))))

(provide 'cat-core)
