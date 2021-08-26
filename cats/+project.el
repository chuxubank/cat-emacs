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
  (+project-find-file-in-dir (car org-agenda-files)))

(defun +find-pdf-files ()
  (interactive)
  (+project-find-file-in-dir "~/PDFs"))

(define-key global-map (kbd "C-c f e") #'+find-emacs-profile)
(define-key global-map (kbd "C-c f p") #'+find-pdf-files)
(define-key global-map (kbd "C-c f o") #'+find-org-files)
(define-key global-map (kbd "C-c p f") #'project-find-file)

;;; recent
(recentf-mode 1)
(defun +find-recentf-open-files ()
  "Use `completing-read' to open a recent file."
  (interactive)
  (let ((files (mapcar 'abbreviate-file-name recentf-list)))
    (find-file (completing-read "Find recent file: " files nil t))))

(define-key global-map (kbd "C-c f r") #'+find-recentf-open-files)
