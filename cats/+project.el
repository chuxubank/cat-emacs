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
(define-key global-map (kbd "C-c n f") #'+find-org-files)
(define-key global-map (kbd "C-c p f") #'project-find-file)
