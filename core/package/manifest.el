;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'package)
(require 'seq)

(defvar package-vc-selected-packages)
(defvar use-package-ensure-function)

(defcustom cat-package-extra-packages nil
  "Packages to keep in addition to packages declared by enabled Cat modules."
  :type '(repeat symbol)
  :group 'cat-emacs)

(defcustom cat-package-extra-vc-packages nil
  "VC package specifications to keep outside enabled Cat modules."
  :type 'sexp
  :group 'cat-emacs)

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

(defun cat-package--without-persisting-selection (function)
  "Call FUNCTION without persisting package selection through Custom."
  (cl-letf (((symbol-function 'package--save-selected-packages)
             (lambda (&optional value)
               (setq package-selected-packages
                     (sort (copy-sequence value) #'string-lessp)))))
    (funcall function)))

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
  (sort (delete-dups
         (append (copy-sequence cat-package-extra-packages)
                 (copy-sequence cat-package--elpa-roots)))
        #'string-lessp))

(defun cat-package-desired-vc-packages ()
  "Return sorted VC roots for the current Cat configuration."
  (let (packages)
    (dolist (spec (append cat-package--bootstrap-vc-roots
                          cat-package-extra-vc-packages
                          cat-package--vc-roots))
      (setq packages (cat-package--put-vc-spec spec packages)))
    (sort packages
          (lambda (left right)
            (string-lessp (car left) (car right))))))

(defun cat-package--set-selection (elpa-packages vc-packages)
  "Set package.el roots to ELPA-PACKAGES and VC-PACKAGES."
  (setq package-selected-packages elpa-packages
        package-vc-selected-packages vc-packages))

(defun cat-package-activate-manifest ()
  "Set package.el's in-memory roots from the active Cat manifest."
  (cat-package--set-selection
   (cat-package-desired-elpa-packages)
   (cat-package-desired-vc-packages)))

(defun cat-package-collect (function)
  "Collect package declarations while calling FUNCTION, then activate them.
Restore the previous manifest and package selection after any nonlocal exit."
  (let ((previous-elpa-roots (copy-sequence cat-package--elpa-roots))
        (previous-vc-roots (copy-tree cat-package--vc-roots))
        (previous-elpa-selection (copy-sequence package-selected-packages))
        (previous-vc-selection (copy-tree package-vc-selected-packages))
        succeeded
        result)
    (cat-package--reset-manifest)
    (unwind-protect
        (progn
          (setq result (funcall function))
          (cat-package-activate-manifest)
          (setq succeeded t)
          result)
      (unless succeeded
        (setq cat-package--elpa-roots previous-elpa-roots
              cat-package--vc-roots previous-vc-roots)
        (cat-package--set-selection
         previous-elpa-selection previous-vc-selection)))))

(defun cat-package--root-package-names (elpa-packages vc-packages)
  "Return package root names from ELPA-PACKAGES and VC-PACKAGES."
  (delete-dups
   (append (copy-sequence elpa-packages)
           (mapcar #'car vc-packages))))

(defun cat-package-sync ()
  "Install and remove packages to match the active Cat configuration.
CI retains the persisted package lists because it does not load Cat modules."
  (interactive)
  (require 'package-vc)
  (let* ((generated-manifest-p
          (bound-and-true-p cat-modules-loaded-p))
         (elpa-packages
          (if generated-manifest-p
              (cat-package-desired-elpa-packages)
            (copy-sequence package-selected-packages)))
         (vc-packages
          (if generated-manifest-p
              (cat-package-desired-vc-packages)
            (copy-tree package-vc-selected-packages)))
         (package-roots
          (cat-package--root-package-names elpa-packages vc-packages)))
    (unless (or generated-manifest-p IS-CI)
      (user-error
       "Cat modules are not loaded; refusing to sync an incomplete manifest"))
    (unless generated-manifest-p
      (message "Using the persisted package manifest for CI"))
    (cat-package--without-persisting-selection
     (lambda ()
       (cat-package--set-selection elpa-packages vc-packages)
       (package-install-selected-packages t)
       (package-vc-install-selected-packages)
       (let ((package-selected-packages package-roots))
         (package-autoremove))))
    ;; Package operations mutate the selected roots; restore the manifest.
    (cat-package--set-selection elpa-packages vc-packages)))

(defun cat-package-upgrade ()
  "Upgrade installed packages without persisting derived package roots."
  (interactive)
  (require 'package-vc)
  (let ((elpa-packages (copy-sequence package-selected-packages))
        (vc-packages (copy-tree package-vc-selected-packages)))
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
    (cat-package--set-selection elpa-packages vc-packages)))

(provide 'cat-package-manifest)
