;; -*- lexical-binding: t; -*-

(defun +add-to-list-multi (list &rest args)
  "Add ARGS to LIST, but only if they are not already in LIST."
  (dolist (arg args)
    (add-to-list list arg)))

(defun +url-get-query-content (key &optional url)
  "Get query value from key in url or clipboard."
  (when (null url)
    (setq url (with-temp-buffer	(clipboard-yank) (buffer-string))))
  (let ((query (cdr (url-path-and-query (url-generic-parse-url url))))
        value)
    (when query
      (if (string-match (format "\\(%s=\\).*?\\(&\\)" key) query)
          (setq value (substring query (match-end 1) (match-beginning 2)))))
    (decode-coding-string (url-unhex-string value) 'utf-8)))


(cl-flet ((always-yes (&rest _) t))
  (defun +no-confirm (fun &rest args)
    "Apply FUN to ARGS, skipping user confirmations."
    (cl-letf (((symbol-function 'y-or-n-p) #'always-yes)
              ((symbol-function 'yes-or-no-p) #'always-yes))
      (apply fun args))))


(defun +dark-mode-p ()
  (eq (frame-parameter nil 'background-mode) 'dark))
