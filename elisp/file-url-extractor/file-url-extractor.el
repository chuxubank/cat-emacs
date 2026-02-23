;;; file-url-extractor.el --- Extract downloadable file URLs -*- lexical-binding: t; -*-

;; Author: Misaka
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience, tools

;;; Commentary:

;; Extract real downloadable file URL from a string.
;;
;; Usage:
;;
;;   (file-url-extractor-get
;;    "some text with url ..."
;;    "pdf")
;;
;; Returns a downloadable URL string.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)

(defgroup file-url-extractor nil
  "Concurrent file URL extractor using Node."
  :group 'tools)

(defcustom file-url-extractor-script-dir
  (expand-file-name "scripts"
                    (file-name-directory
                     (or load-file-name buffer-file-name)))
  "Directory containing Node resolver scripts.
Each script must be named by domain:
  nnfp.jss.com.cn.ts"
  :type 'directory)

(defcustom file-url-extractor-node-command "bun"
  "Node executable."
  :type 'string)

;; ------------------------------------------------------------
;; Utilities
;; ------------------------------------------------------------

(defun file-url-extractor--collect-urls (text)
  (let (result)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward
              "\\(https?://[[:alnum:][:punct:]]+\\)"
              nil t)
        (push (url-unhex-string (match-string 1)) result)))
    (cl-remove-duplicates result :test #'equal)))

(defun file-url-extractor--domain (url)
  (url-host (url-generic-parse-url url)))

(defun file-url-extractor--script-for (domain)
  (when file-url-extractor-script-dir
    (let* ((base (expand-file-name domain
                                   file-url-extractor-script-dir))
           (ts (concat base ".ts"))
           (js (concat base ".js")))
      (cond
       ((file-exists-p ts) ts)
       ((file-exists-p js) js)
       (t nil)))))

(defun file-url-extractor--run-node (script url file-ext)
  (with-temp-buffer
    (if (eq 0 (call-process
               file-url-extractor-node-command
               nil t nil
               "run" script url file-ext))
        (string-trim (buffer-string))
      nil)))

(defun file-url-extractor--extension-match-p (url ext)
  (string-match-p
   (format "\\.%s\\($\\|\\?\\)" (regexp-quote ext))
   url))

(defun file-url-extractor--content-type-match-p (url ext)
  (with-temp-buffer
    (when (eq 0 (call-process "curl" nil t nil "-sI" "-L" url))
      (goto-char (point-min))
      (re-search-forward
       (format "Content-Type:.*%s" ext)
       nil t))))

(defun file-url-extractor--resolve-candidate (url file-ext)
  "Resolve a single URL to a file, using domain script if available.
If a script exists for the URL's domain, use it directly.
Otherwise, check extension or content-type match with FILE-EXT."
  (let* ((domain (file-url-extractor--domain url))
         (script (file-url-extractor--script-for domain)))
    (if script
        (file-url-extractor--run-node script url file-ext)
      (when (or (file-url-extractor--extension-match-p url file-ext)
                (file-url-extractor--content-type-match-p url file-ext))
        url))))

;; ------------------------------------------------------------
;; Parallel execution
;; ------------------------------------------------------------

(defun file-url-extractor--parallel-resolve (urls file-ext)
  (let* ((results '())
         (mutex (make-mutex))
         (done 0)
         (total (length urls))
         (cv (make-condition-variable mutex)))

    (dolist (url urls)
      (make-thread
       (lambda ()
         (let ((res (ignore-errors
                      (file-url-extractor--resolve-candidate
                       url file-ext))))
           (with-mutex mutex
             (when res
               (push res results))
             (setq done (1+ done))
             (condition-notify cv))))))

    (with-mutex mutex
      (while (< done total)
        (condition-wait cv)))

    (nreverse results)))

;; ------------------------------------------------------------
;; Public API
;; ------------------------------------------------------------

(defun file-url-extractor-get-all (input file-ext)
  (unless file-url-extractor-script-dir
    (error "file-url-extractor-script-dir not configured"))
  (let ((urls (if (listp input)
                  input
                (file-url-extractor--collect-urls input))))
    (file-url-extractor--parallel-resolve urls file-ext)))

(defun file-url-extractor-get (input file-ext)
  (car (file-url-extractor-get-all input file-ext)))

(provide 'file-url-extractor)

;;; file-url-extractor.el ends here
