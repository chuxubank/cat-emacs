;; -*- lexical-binding: t; -*-

(require 'package)

(defun cat/package--read-package (prompt &optional selected-only)
  "Read a package name with PROMPT.
When SELECTED-ONLY is non-nil, only read from `package-selected-packages'."
  (let* ((packages (if selected-only
                       package-selected-packages
                     (append package-selected-packages
                             (mapcar #'car package-alist)
                             (mapcar #'car package-archive-contents))))
         (names (sort (delete-dups (mapcar #'symbol-name packages)) #'string<)))
    (when (and selected-only (null names))
      (user-error "No selected packages"))
    (let ((name (completing-read prompt names nil selected-only)))
      (when (string-empty-p name)
        (user-error "Package name cannot be empty"))
      (intern name))))

(defun cat/package-select (package)
  "Add PACKAGE to `package-selected-packages' and save it."
  (interactive (list (cat/package--read-package "Select package: ")))
  (unless (memq package package-selected-packages)
    (package--save-selected-packages (cons package package-selected-packages)))
  (message "Selected package: %s" package))

(defun cat/package-unselect (package)
  "Remove PACKAGE from `package-selected-packages' and save it."
  (interactive (list (cat/package--read-package "Unselect package: " t)))
  (package--save-selected-packages (delq package (copy-sequence package-selected-packages)))
  (message "Unselected package: %s" package))

(defun cat/package-edit-selected-packages (action package)
  "Manually add or remove PACKAGE from `package-selected-packages'."
  (interactive
   (let ((action (intern (completing-read "Action: " '("add" "remove") nil t))))
     (list action
           (cat/package--read-package
            (pcase action
              ('add "Select package: ")
              ('remove "Unselect package: "))
            (eq action 'remove)))))
  (pcase action
    ('add (cat/package-select package))
    ('remove (cat/package-unselect package))))
