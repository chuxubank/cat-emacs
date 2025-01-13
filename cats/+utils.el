;; -*- lexical-binding: t; -*-

(defun +add-to-list-multi (list &rest args)
  "Add ARGS to LIST, but only if they are not already in LIST."
  (dolist (arg args)
    (add-to-list list arg)))

(defun +random-get (list)
  "Random get item from LIST."
  (nth (random (length list)) list))

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

(defun +with-icon (icon &optional str)
  "Combine ICON with STR."
  (let* ((category (nth 1 (split-string icon "-")))
         (nd-icon (pcase category
                    ("oct" (nerd-icons-octicon icon))
                    ("md" (nerd-icons-mdicon icon))
                    ("weather" (nerd-icons-wicon icon))
                    ("linux" (nerd-icons-flicon icon))
                    ("cod" (nerd-icons-codicon icon))
                    ("fa" (nerd-icons-faicon icon))
                    ("dev" (nerd-icons-devicon icon))
                    ("ice" (nerd-icons-ipsicon icon))
                    ("pom" (nerd-icons-pomicon icon))
                    ("seti" (nerd-icons-sucicon icon))
                    ("custom" (nerd-icons-sucicon icon))
                    ("pl" (nerd-icons-powerline icon)))))
    (if (or (null str)
            (string-empty-p str))
        nd-icon
      (concat nd-icon " " str))))

(defun +with-mode-icon (mode &optional str)
  "Give icon for MODE and STR."
  (let ((nd-icon (nerd-icons-icon-for-mode mode)))
    (if (or (null str)
            (string-empty-p str))
        nd-icon
      (concat nd-icon " " str))))

(defun +project-find-file-in-dir (dir &optional exclude-dirs include-all)
  "Find a file in a specific directory DIR, optionally excluding certain paths.

EXCLUDE-DIRS is a list of paths to exclude from the search.

If INCLUDE-ALL is non-nil, include all files from DIRS, except for VCS
directories listed in `vc-directory-exclusion-list'."
  (let* ((vc-directory-exclusion-list (append vc-directory-exclusion-list (ensure-list exclude-dirs)))
         (pr (project-current nil dir)))
    (if pr
        (project-find-file-in nil (ensure-list dir) pr include-all)
      (user-error "Not a project directory: %s" dir))))

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

(defun +find-emacs-profile ()
  "Find a file in the Emacs user directory, excluding specified directories."
  (interactive)
  (let ((exclude-dirs (list ".local"
                            "elpa"
                            "eln-cache"
                            "tree-sitter")))
    (+project-find-file-in-dir
     user-emacs-directory
     exclude-dirs
     t)))

(defun +emacs-debug-init ()
  "Start Emacs in debug mode."
  (interactive)
  (type-break-mode -1)
  (shell-command "emacs --debug")
  (type-break-mode 1))
