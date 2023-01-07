;; -*- lexical-binding: t; -*-

(defun +project-root ()
  (when-let (project (project-current))
    (car (project-roots project))))

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

(defvar cat-file-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" #'+delete-file-and-buffer)
    (define-key map "e" #'+find-emacs-profile)
    (define-key map "f" #'find-function)
    (define-key map "l" #'find-library)
    (define-key map "o" #'+find-org-files)
    (define-key map "r" #'recentf-open-files)
    map)
  "Keymap for file commands.")
(defalias 'cat-file-prefix cat-file-map)
