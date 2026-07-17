;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar cat-current-module nil
  "Current Cat module being loaded.")

(defvar cat-current-module-group nil
  "Current Cat module group being loaded.")

(defvar cat-current-module-options nil
  "Options for the current Cat module.")

(defvar cat-module-options nil
  "Alist of Cat module options grouped by module group.")

(defvar cat-modules-enabled nil
  "Alist of enabled Cat modules grouped by module group.")

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

(defun cat--query-symbol-form (value)
  "Return a query form for VALUE, treating bare symbols as quoted names."
  (cond
   ((null value) nil)
   ((symbolp value) `',value)
   (t value)))

(defun cat--query-group-form (group)
  "Return a query form for GROUP, treating bare symbols as group names."
  (cond
   ((null group) nil)
   ((keywordp group) (substring (symbol-name group) 1))
   ((symbolp group) (symbol-name group))
   (t group)))

(defmacro catp! (cat &optional module group)
  "Return non-nil when FEATURE is enabled for MODULE in GROUP.
When MODULE and GROUP are omitted, use the module currently being loaded."
  `(memq ',cat
         (ensure-list
          (cat--module-option :cat
                              ,(cat--query-symbol-form module)
                              ,(cat--query-group-form group)))))

(defmacro modulep! (module &optional group)
  "Return non-nil when MODULE is enabled in GROUP.
When GROUP is omitted, check every module group."
  (if group
      `(memq ',module
             (alist-get ,(cat--query-group-form group)
                        cat-modules-enabled nil nil #'equal))
    `(cl-some (lambda (modules) (memq ',module (cdr modules)))
              cat-modules-enabled)))

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

(defun cat-load (module group &optional noerror)
  "Load MODULE from GROUP under the modules directory."
  (let ((file (expand-file-name
               module
               (expand-file-name group (expand-file-name "modules" user-emacs-directory)))))
    (cat-load-file file "module" noerror)))

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
  (let ((modules (alist-get group cat-modules-enabled nil nil #'equal)))
    (setf (alist-get group cat-modules-enabled nil nil #'equal)
          (cl-adjoin module modules)))
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

(defun cat-load-modules (&optional modules-file)
  "Load Cat module declarations from MODULES-FILE.
When MODULES-FILE is nil, load the configured cats file."
  (load (or modules-file (cat-config-file "cats")) nil 'nomessage)
  (cat! cat-modules))

(provide 'cat-module)
