;; -*- lexical-binding: t; -*-

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
