(defun find-profile ()
  (interactive)
  (let ((project (project--find-in-directory user-emacs-directory)))
    (project-find-file-in nil (project-roots project) project)))
