;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'package)
(require 'seq)

(defvar package-vc-selected-packages nil)
(defvar use-package-ensure-function)

(defconst cat-package-manifest-format-version 1
  "Current generated package manifest format version.")

(defvar cat-package-manifest-state 'empty
  "Current package manifest state.
The value is one of `empty', `collecting', `ready', or `failed'.")

(defvar cat-package--elpa-roots nil
  "ELPA packages declared by enabled Cat modules.")

(defvar cat-package--vc-roots nil
  "VC package specifications declared by enabled Cat modules.")

(defvar cat-package--bootstrap-vc-roots nil
  "VC packages required while bootstrapping Cat Emacs.")

(defvar cat-package--ensure-function #'use-package-ensure-elpa
  "Original function used by `cat-package--use-package-ensure'.")

(defun cat-package--reset-manifest ()
  "Clear package roots collected from Cat modules."
  (setq cat-package--elpa-roots nil
        cat-package--vc-roots nil))

(defun cat-package--register-elpa (package)
  "Register PACKAGE as an ELPA root unless it is built in."
  (when (and package
             (not (package-built-in-p package)))
    (cl-pushnew package cat-package--elpa-roots)))

(defun cat-package--put-vc-spec (spec packages)
  "Return PACKAGES with VC package SPEC inserted or replaced."
  (cons spec (assq-delete-all (car spec) packages)))

(defun cat-package--register-vc (spec)
  "Register package-vc SPEC as a module package root."
  (setq cat-package--vc-roots
        (cat-package--put-vc-spec spec cat-package--vc-roots)))

(defun cat-package--register-bootstrap-vc (spec)
  "Register package-vc SPEC as a bootstrap package root."
  (setq cat-package--bootstrap-vc-roots
        (cat-package--put-vc-spec spec cat-package--bootstrap-vc-roots)))

(defun cat-package--set-custom-value (variable value)
  "Set Custom VARIABLE to VALUE without saving it."
  (funcall (or (get variable 'custom-set) #'set-default)
           variable value)
  value)

(defun cat-package--without-persisting-selection (function)
  "Call FUNCTION without persisting package selection through Custom.
In-memory changes to package selection variables are preserved."
  (require 'cus-edit)
  (let ((save-variable (symbol-function 'customize-save-variable)))
    (cl-letf (((symbol-function 'package--save-selected-packages)
               (lambda (&optional value)
                 (when (or value after-init-time)
                   (setq package-selected-packages
                         (sort (copy-sequence value)
                               #'string-lessp)))))
              ((symbol-function 'customize-save-variable)
               (lambda (variable value &optional comment)
                 (if (memq variable
                           '(package-selected-packages
                             package-vc-selected-packages))
                     (cat-package--set-custom-value variable value)
                   (funcall save-variable variable value comment)))))
      (funcall function))))

(defun cat-package-install (package)
  "Install PACKAGE without persisting derived package selection."
  (cat-package--without-persisting-selection
   (lambda ()
     (package-install package))))

(defun cat-package-vc-install (spec)
  "Install package-vc SPEC without persisting derived package selection."
  (require 'package-vc)
  (cat-package--without-persisting-selection
   (lambda ()
     (package-vc-install spec))))

(defun cat-package--use-package-ensure (name args state)
  "Record use-package NAME with ensure ARGS, then call the package backend.
STATE is the normalized use-package state."
  (dolist (ensure args)
    (let ((package (if (eq ensure t) name ensure)))
      (when (consp package)
        (setq package (car package)))
      (cat-package--register-elpa package)))
  (cat-package--without-persisting-selection
   (lambda ()
     (funcall cat-package--ensure-function name args state))))

(defun cat-package--vc-spec (arg)
  "Convert normalized use-package VC ARG to a package-vc specification."
  (pcase-let* ((`(,name ,options ,revision) arg)
               (spec
                (cond
                 ((and (listp options)
                       (stringp revision)
                       (not (plist-member options :branch)))
                  (append options (list :branch revision)))
                 ((and (null options) (stringp revision))
                  revision)
                 (t options))))
    (cons name spec)))

(defun cat-package--use-package-vc-install-around
    (original arg &optional local-path)
  "Record use-package VC ARG, then call ORIGINAL with LOCAL-PATH."
  (unless local-path
    (cat-package--register-vc (cat-package--vc-spec arg)))
  (cat-package--without-persisting-selection
   (lambda ()
     (funcall original arg local-path))))

(defun cat-package--install-use-package-hooks ()
  "Install package manifest hooks for use-package."
  (unless (eq use-package-ensure-function
              #'cat-package--use-package-ensure)
    (setq cat-package--ensure-function use-package-ensure-function
          use-package-ensure-function #'cat-package--use-package-ensure)))

(eval-after-load 'use-package-ensure
  '(cat-package--install-use-package-hooks))

(eval-after-load 'use-package-core
  '(unless (advice-member-p
            #'cat-package--use-package-vc-install-around
            'use-package-vc-install)
     (advice-add 'use-package-vc-install :around
                 #'cat-package--use-package-vc-install-around)))

(defun cat-package-desired-elpa-packages ()
  "Return sorted ELPA roots for the current Cat configuration."
  (sort (delete-dups (copy-sequence cat-package--elpa-roots))
        #'string-lessp))

(defun cat-package--normalize-vc-packages (specs)
  "Return sorted VC package SPECS with duplicate names replaced."
  (let (packages)
    (dolist (spec specs)
      (setq packages (cat-package--put-vc-spec spec packages)))
    (sort packages
          (lambda (left right)
            (string-lessp (car left) (car right))))))

(defun cat-package-desired-vc-packages ()
  "Return sorted VC roots for the current Cat configuration."
  (cat-package--normalize-vc-packages
   (append cat-package--bootstrap-vc-roots
           cat-package--vc-roots)))

(defun cat-package--root-package-names (elpa-packages vc-packages)
  "Return package root names from ELPA-PACKAGES and VC-PACKAGES."
  (sort
   (delete-dups
    (append (copy-sequence elpa-packages)
            (mapcar #'car vc-packages)))
   #'string-lessp))

(defun cat-package--activate-manifest ()
  "Set package.el's in-memory roots from the collected manifest."
  (let ((elpa-packages (cat-package-desired-elpa-packages))
        (vc-packages (cat-package-desired-vc-packages)))
    (setq package-selected-packages
          (cat-package--root-package-names elpa-packages vc-packages)
          package-vc-selected-packages vc-packages)))

(defun cat-package-collect (function)
  "Collect package declarations while calling FUNCTION, then activate them.
Restore the previous manifest, state, and package selection after any
nonlocal exit."
  (when (eq cat-package-manifest-state 'collecting)
    (error "Recursive package manifest collection"))
  (let ((previous-state cat-package-manifest-state)
        (previous-elpa-roots (copy-sequence cat-package--elpa-roots))
        (previous-vc-roots (copy-tree cat-package--vc-roots))
        (previous-selection (copy-sequence package-selected-packages))
        (previous-vc-selection (copy-tree package-vc-selected-packages))
        succeeded
        result)
    (setq cat-package-manifest-state 'collecting)
    (cat-package--reset-manifest)
    (unwind-protect
        (condition-case err
            (progn
              (setq result (funcall function))
              (cat-package--activate-manifest)
              (setq cat-package-manifest-state 'ready
                    succeeded t)
              result)
          (error
           (setq cat-package-manifest-state 'failed)
           (signal (car err) (cdr err))))
      (unless succeeded
        (setq cat-package-manifest-state previous-state
              cat-package--elpa-roots previous-elpa-roots
              cat-package--vc-roots previous-vc-roots
              package-selected-packages previous-selection
              package-vc-selected-packages previous-vc-selection)))))

(defun cat-package--require-ready-manifest ()
  "Signal a user error unless the package manifest is ready."
  (unless (eq cat-package-manifest-state 'ready)
    (user-error "Package manifest is %s; load the configured cats first"
                cat-package-manifest-state)))

(defun cat-package--manifest-data ()
  "Return the active package manifest as versioned Lisp data."
  (cat-package--require-ready-manifest)
  (list :format-version cat-package-manifest-format-version
        :elpa (cat-package-desired-elpa-packages)
        :vc (cat-package-desired-vc-packages)))

(defun cat-package-write-manifest (file)
  "Write the active derived package manifest to FILE.
The generated data is intended for build caching, not manual maintenance."
  (interactive "FWrite package manifest to file: ")
  (let ((path (expand-file-name file))
        (coding-system-for-write 'utf-8-unix)
        (print-circle nil)
        (print-gensym nil)
        (print-length nil)
        (print-level nil))
    (make-directory (file-name-directory path) t)
    (with-temp-file path
      (prin1 (cat-package--manifest-data) (current-buffer))
      (terpri (current-buffer)))
    path))

(defun cat-package--read-manifest-data (file)
  "Read the single generated package manifest form from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let* ((read-eval nil)
           (data (read (current-buffer))))
      (skip-chars-forward " \t\r\n")
      (unless (eobp)
        (error "Unexpected trailing data in package manifest %s" file))
      data)))

(defun cat-package--validate-manifest-data (data file)
  "Validate package manifest DATA read from FILE and return it."
  (unless (proper-list-p data)
    (error "Invalid package manifest %s" file))
  (let ((elpa-packages (plist-get data :elpa))
        (vc-packages (plist-get data :vc)))
    (unless (and (equal (plist-get data :format-version)
                        cat-package-manifest-format-version)
                 (plist-member data :elpa)
                 (proper-list-p elpa-packages)
                 (seq-every-p #'symbolp elpa-packages)
                 (plist-member data :vc)
                 (proper-list-p vc-packages)
                 (seq-every-p
                  (lambda (spec)
                    (and (consp spec)
                         (symbolp (car spec))
                         (let ((value (cdr spec)))
                           (or (null value)
                               (stringp value)
                               (proper-list-p value)))))
                  vc-packages))
      (error "Invalid or unsupported package manifest %s" file))
    data))

(defun cat-package-load-manifest (file)
  "Activate the generated package manifest in FILE."
  (interactive "fLoad package manifest: ")
  (let* ((data
          (cat-package--validate-manifest-data
           (cat-package--read-manifest-data file)
           file))
         (elpa-packages
          (sort (delete-dups
                 (copy-sequence (plist-get data :elpa)))
                #'string-lessp))
         (vc-packages
          (cat-package--normalize-vc-packages
           (copy-tree (plist-get data :vc)))))
    (cat-package-collect
     (lambda ()
       (setq cat-package--elpa-roots elpa-packages
             cat-package--vc-roots vc-packages)))))

(defun cat-package-sync ()
  "Install and remove packages to match the active Cat configuration."
  (interactive)
  (require 'package-vc)
  (cat-package--require-ready-manifest)
  (let* ((elpa-packages (cat-package-desired-elpa-packages))
         (vc-packages (cat-package-desired-vc-packages))
         (package-roots
          (cat-package--root-package-names elpa-packages vc-packages))
         (previous-selection (copy-sequence package-selected-packages))
         (previous-vc-selection (copy-tree package-vc-selected-packages)))
    (unwind-protect
        (cat-package--without-persisting-selection
         (lambda ()
           (let ((package-selected-packages elpa-packages))
             (package-install-selected-packages t))
           (let ((package-vc-selected-packages vc-packages))
             (package-vc-install-selected-packages))
           (let ((package-selected-packages package-roots))
             (package-autoremove))))
      (setq package-selected-packages previous-selection
            package-vc-selected-packages previous-vc-selection))))

(defun cat-package-upgrade ()
  "Upgrade installed packages without persisting derived package roots."
  (interactive)
  (require 'package-vc)
  (cat-package--require-ready-manifest)
  (let ((previous-selection (copy-sequence package-selected-packages))
        (previous-vc-selection (copy-tree package-vc-selected-packages)))
    (unwind-protect
        (cat-package--without-persisting-selection
         (lambda ()
           ;; Emacs 30 includes VC descriptors in `package-upgrade-all';
           ;; upgrade them once through package-vc below.
           (let ((package-alist
                  (seq-remove
                   (lambda (entry)
                     (seq-some #'package-vc-p (cdr entry)))
                   package-alist)))
             (package-upgrade-all nil))
           (package-vc-upgrade-all)))
      (setq package-selected-packages previous-selection
            package-vc-selected-packages previous-vc-selection))))

(provide 'cat-package-manifest)
