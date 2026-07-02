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

(defcustom cat-user-directory
  (expand-file-name "cat-emacs/" (or (getenv "XDG_CONFIG_HOME") "~/.config"))
  "Directory for Cat Emacs user configuration."
  :type 'directory
  :group 'cat-emacs)

(defun cat-user-file (file)
  "Return FILE under `cat-user-directory'."
  (expand-file-name file cat-user-directory))

(defun cat-template-file (file)
  "Return FILE under the Cat Emacs templates directory."
  (expand-file-name file (expand-file-name "templates" user-emacs-directory)))

(defun cat-config-file (file)
  "Return user FILE if it exists, otherwise return the template FILE."
  (let ((user-file (cat-user-file file)))
    (if (file-exists-p user-file)
        user-file
      (cat-template-file file))))

(defun cat-config-directory (directory)
  "Return user DIRECTORY if it exists, otherwise return the template DIRECTORY."
  (let ((user-directory (file-name-as-directory (cat-user-file directory))))
    (if (file-directory-p user-directory)
        user-directory
      (file-name-as-directory (cat-template-file directory)))))

(defvar cat-prompt-dir (cat-config-directory "prompt")
  "Directory for prompt templates.")

(defvar cat-custom-reevaluate-setting-list nil
  "List of custom settings to reevaluate for each client frame.
Each element is a variable passed to `custom-reevaluate-setting'.")

(defun cat-custom-reevaluate-settings ()
  "Reevaluate settings in `cat-custom-reevaluate-setting-list'."
  (dolist (setting cat-custom-reevaluate-setting-list)
    (custom-reevaluate-setting setting)))

;;; load-path
(let ((default-directory (expand-file-name "elisp" user-emacs-directory)))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

;;; modules
(load (expand-file-name "core/core" user-emacs-directory) nil 'nomessage)
(load (cat-config-file "cats") nil 'nomessage)
(cat! cat-modules)

;;; ui
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; enable
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'list-threads 'disabled nil)
(put 'magit-clean 'disabled nil)

(cat-benchmark 'end)
