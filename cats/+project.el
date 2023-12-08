;; -*- lexical-binding: t; -*-

(use-package project
  :commands #'project--find-in-directory
  :custom
  (project-list-file (concat cat-cache-dir "projects")))

(defun +project-find-file-in-dir (dir)
  (let* ((pr (or (project--find-in-directory dir)
                 (cons 'transient dir)))
         (dirs (project-roots pr)))
    (project-find-file-in nil dirs pr)))

(defun +find-emacs-profile ()
  (interactive)
  (+project-find-file-in-dir user-emacs-directory))

(defun +find-org-files ()
  (interactive)
  (+project-find-file-in-dir cat-org-directory))

(defun +delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (if (y-or-n-p (concat "Do you really want to delete file " filename " ?"))
            (progn
              (delete-file filename)
              (message "Deleted file %s." filename)
              (kill-buffer)))
      (message "Not a file visiting buffer!"))))

(defvar-keymap cat-file-map
  :doc "Keymap for file commands."
  :name "Cat File"
  :prefix 'cat-file-prefix
  "d" #'+delete-file-and-buffer
  "e" #'+find-emacs-profile
  "f" #'find-function
  "l" #'find-library
  "o" #'+find-org-files
  "O" #'consult-org-agenda
  "r" #'recentf-open-files)
