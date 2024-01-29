;; -*- lexical-binding: t; -*-

(defun +add-to-list-multi (list &rest args)
  "Add ARGS to LIST, but only if they are not already in LIST."
  (dolist (arg args)
    (add-to-list list arg)))

(defun +url-get-query-content (key &optional url)
  "Get query value from key in url or clipboard."
  (when (null url)
    (setq url (with-temp-buffer (clipboard-yank) (buffer-string))))
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
  "Detect whether current frame is dark mode."
  (eq (frame-parameter nil 'background-mode) 'dark))

(defun +start-process-with-finish-callback (process-name buffer command callback)
  "Start an asynchronous process with a given CALLBACK function as its finish sentinel."
  (let ((process (apply #'start-process process-name buffer command)))
    (set-process-sentinel
     process
     (lambda (proc event)
       (when (string-match-p "finished" event)
         (funcall callback (process-exit-status proc)))))
    process))

(defun +change-lighter (&rest list)
  "Change a modeline lighter for given minor modes.
List contains pairs mode lighter, see `minor-mode-alist'"
  (let (output)
    (while list
      (let ((mode (car list))
	        (newlighter (nth 1 list)))
        (setcar (cdr (assq mode minor-mode-alist)) newlighter))
      (setq list (nthcdr 2 list)))
    (reverse output)))

(defun +mkdir-p (dir)
  "Make directory for DIR if not exists."
  (unless (file-directory-p dir)
    (make-directory dir)))

(defun +emacs-debug-init ()
  "Start Emacs in debug mode."
  (interactive)
  (type-break-mode -1)
  (shell-command "emacs --debug")
  (type-break-mode 1))
