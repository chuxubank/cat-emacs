;; -*- lexical-binding: t; -*-

(defvar cat-current-module nil
  "Current Cat module being loaded.")

(defvar cat-current-module-group nil
  "Current Cat module group being loaded.")

(defvar cat-current-module-options nil
  "Options for the current Cat module.")

(defvar cat-module-options nil
  "Alist of Cat module options grouped by module group.")

(defun cat--plist-get (plist prop)
  "Return PLIST value for PROP, preserving explicit nil values."
  (when (memq prop plist)
    (plist-get plist prop)))

(defun cat--module-option (option &optional module group)
  "Return OPTION for MODULE in GROUP.
When MODULE and GROUP are nil, use the module currently being loaded."
  (if (or module group)
      (let* ((module-name (or module cat-current-module))
             (group-name (or group cat-current-module-group))
             (options (alist-get module-name
                                 (alist-get group-name cat-module-options nil nil #'equal))))
        (cat--plist-get options option))
    (cat--plist-get cat-current-module-options option)))

(defun cat-feature-enabled-p (feature &optional module group)
  "Return non-nil when FEATURE is enabled for MODULE in GROUP.
Module options support a `:feature' allow-list.  Optional package declarations
marked with `use-package' `:feature' are disabled unless their feature appears
in the current module's `:feature' list."
  (let ((features (cat--module-option :feature module group)))
    (memq feature features)))

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

(defun cat--module-declaration-p (module)
  "Return non-nil when MODULE is a module declaration with options."
  (and (consp module)
       (symbolp (car module))
       (not (keywordp (car module)))))

(defun cat--module-options (module)
  "Return the options plist for MODULE."
  (if (cat--module-declaration-p module)
      (cdr module)
    nil))

(defun cat--module-symbol (module)
  "Return the module symbol from MODULE."
  (if (cat--module-declaration-p module)
      (car module)
    module))

(defun cat--register-module-options (group module options)
  "Register OPTIONS for MODULE in GROUP."
  (when options
    (let ((group-options (alist-get group cat-module-options nil nil #'equal)))
      (setf (alist-get module group-options nil nil #'equal) options)
      (setf (alist-get group cat-module-options nil nil #'equal) group-options))))

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
      (let* ((module-name (cat--module-symbol module))
             (module-options (cat--module-options module)))
        (cat--register-module-options group module-name module-options)
        (let ((cat-current-module module-name)
              (cat-current-module-group group)
              (cat-current-module-options module-options))
          (cat-load (cat--module-name module-name) group)))))))

(provide 'cat-module)
