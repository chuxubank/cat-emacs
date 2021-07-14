;;; modules
(defmacro cat! (filename &optional path noerror)
  "Load a module in cats by default"
  (let* ((path (or path
		   (expand-file-name "cats" user-emacs-directory)
                   (error "Could not detect path to look for '%s' in"
                          filename)))
         (file (if path
                  `(expand-file-name ,filename ,path)
                filename)))
    `(condition-case-unless-debug e
         (let (file-name-handler-alist)
           (load ,file ,noerror 'nomessage))
       (error "Could not load file '%s'" file))))

(load (concat user-emacs-directory "config") nil 'nomessage)
