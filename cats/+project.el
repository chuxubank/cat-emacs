;; -*- lexical-binding: t; -*-

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

(defun +find-pdf-files ()
  (interactive)
  (+project-find-file-in-dir "~/PDFs"))

(defun +find-recentf-open-files ()
  "Use `completing-read' to open a recent file."
  (interactive)
  (let ((files (mapcar 'abbreviate-file-name recentf-list)))
    (find-file (completing-read "Find recent file: " files nil t))))

(defun +delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (if (y-or-n-p (concat "Do you really want to delete file " filename " ?"))
            (progn
              (move-file-to-trash filename)
              (message "Deleted file %s." filename)
              (kill-buffer)))
      (message "Not a file visiting buffer!"))))

(define-key global-map (kbd "C-c f e") #'+find-emacs-profile)
(define-key global-map (kbd "C-c f p") #'+find-pdf-files)
(define-key global-map (kbd "C-c f o") #'+find-org-files)
(define-key global-map (kbd "C-c f r") #'+find-recentf-open-files)
(define-key global-map (kbd "C-c f l") #'find-library)
(define-key global-map (kbd "C-c f d") #'+delete-file-and-buffer)
(define-key global-map (kbd "C-c p f") #'project-find-file)
