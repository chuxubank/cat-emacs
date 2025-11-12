;; -*- lexical-binding: t; -*-

(cat-benchmark 'beg)

;;; custom
(setq custom-file "~/.config/emacs-custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(add-hook 'kill-emacs-query-functions
          'custom-prompt-customize-unsaved-options)

(defun custom-unsaved-options--only-variables-p (result vars)
  "Return non-nil if RESULT only contain entries whose symbols are in VARS.
VARS can be a symbol or a list of symbols."
  (let ((vars (if (symbolp vars) (list vars) vars)))
    (when result
      (let ((symbols (mapcar #'car result)))
        (and (= (length symbols) (length vars))
             (null (cl-set-difference symbols vars)))))))

(defun custom-unsaved-options--around (orig-fn &rest args)
  "Advice around `custom-unsaved-options' to suppress trivial ORIG-FN results with ARGS."
  (let ((result (apply orig-fn args)))
    (if (custom-unsaved-options--only-variables-p
         result '(mouse-wheel-progressive-speed))
        nil
      result)))

(advice-add 'custom-unsaved-options :around #'custom-unsaved-options--around)

(defgroup cat-emacs nil
  "A lightweight Emacs configuration works on Linux, macOS and Windows."
  :group 'Emacs
  :version "29.3")

(defcustom cat-org-directory
  (or (getenv "ORG_DIR") "~/Developer/Personal/org/")
  "Filename of the org folder.
See `org-directory'."
  :type 'directory
  :group 'cat-emacs)

(defcustom cat-pass-directory
  (or (getenv "PASSWORD_STORE_DIR") "~/.password-store")
  "Filename of the password-store folder.
See `auth-source-pass-filename'."
  :type 'directory
  :group 'cat-emacs)

;;; packages
(let ((default-directory (expand-file-name "elisp" user-emacs-directory)))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

;;; modules
(defmacro cat! (filename &optional path noerror)
  "Load a module in cats by default"
  (let* ((path (or path
                   (expand-file-name "cats" user-emacs-directory)
                   (error "Could not detect path to look for '%s' in"
                          filename)))
         (file (if path
                   `(expand-file-name ,filename ,path)
                 filename)))
    `(condition-case-unless-debug err
         (let (file-name-handler-alist)
           (cat-benchmark 'beg ,file)
           (load ,file ,noerror 'nomessage))
       (error
        (message "ERROR: %S when loading file: %s\nBacktrace:\n%s"
                 err
                 (abbreviate-file-name ,file)
                 (with-output-to-string (backtrace)))))))

(load (concat user-emacs-directory "config") nil 'nomessage)

;;; ui
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; enable
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'list-threads 'disabled nil)
(put 'magit-clean 'disabled nil)

(cat-benchmark 'end)
