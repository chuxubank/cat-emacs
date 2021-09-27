;;; custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;;;###autoload
(defun cat-call-process (command &rest args)
  "Execute COMMAND with ARGS synchronously.

Returns (STATUS . OUTPUT) when it is done, where STATUS is the returned error
code of the process and OUTPUT is its stdout output."
  (with-temp-buffer
    (cons (or (apply #'call-process command nil t nil (remq nil args))
              -1)
          (string-trim (buffer-string)))))

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
