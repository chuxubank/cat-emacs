;; -*- lexical-binding: t; -*-

(cat-benchmark 'beg)

;;; custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;;; packages
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

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

;;; enable
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'list-threads 'disabled nil)
(put 'magit-clean 'disabled nil)

(cat-benchmark 'end)
