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

;;; packages
(let ((default-directory (expand-file-name "elisp" user-emacs-directory)))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

;;; modules
(defun cat-load (module group &optional noerror)
  "Load MODULE from GROUP under the modules directory."
  (let ((file (expand-file-name
               module
               (expand-file-name group (expand-file-name "modules" user-emacs-directory)))))
    (condition-case-unless-debug err
         (let (file-name-handler-alist)
           (cat-benchmark 'beg file)
           (load file noerror 'nomessage))
       (error
        (message "ERROR: %S when loading file: %s\nBacktrace:\n%s"
                 err
                 (abbreviate-file-name file)
                 (with-output-to-string (backtrace)))))))

(defun cat--module-name (module)
  "Return the file name for MODULE."
  (cond
   ((stringp module) module)
   ((symbolp module) (concat "+" (symbol-name module)))
   (t (error "Invalid Cat module: %S" module))))

(defun cat--module-group (group)
  "Return the directory name for GROUP."
  (substring (symbol-name group) 1))

(defun cat! (modules &optional group)
  "Load MODULES with grouped declarations like Doom's `doom!':

  (cat! '(:ui doom font
          :editor meow avy))"
  (dolist (module modules)
    (cond
     ((keywordp module)
      (setq group (cat--module-group module)))
     ((and (consp module) (eq (car module) :if))
      (when (eval (cadr module) lexical-binding)
        (cat! (cddr module) group)))
     (t
      (unless group
        (error "Cat module %S has no group" module))
      (cat-load (cat--module-name module) group)))))

(let ((user-cats-file (expand-file-name "cats" cat-user-directory))
      (default-cats-file (expand-file-name "templates/cats.example" user-emacs-directory)))
  (load (if (file-exists-p (concat user-cats-file ".el"))
            user-cats-file
          default-cats-file)
        nil 'nomessage))
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
