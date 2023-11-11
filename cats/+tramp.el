;; -*- lexical-binding: t; -*-

(defun +sudo-edit-current-file ()
  (interactive)
  (let ((my-file-name)
        (position))
    (if (equal major-mode 'dired-mode)
        (progn
          (setq my-file-name (dired-get-file-for-visit))
          (find-alternate-file (prepare-tramp-sudo-string my-file-name)))
      (setq my-file-name (buffer-file-name)
            position (point))
      (find-alternate-file (prepare-tramp-sudo-string my-file-name))
      (goto-char position))))

(defun prepare-tramp-sudo-string (tempfile)
  (if (file-remote-p tempfile)
      (let ((vec (tramp-dissect-file-name tempfile)))
        (tramp-make-tramp-file-name
         "sudo"
         ""
         (tramp-file-name-domain vec)
         (tramp-file-name-host vec)
         (tramp-file-name-port vec)
         (tramp-file-name-localname vec)
         (format "ssh:%s@%s|"
                 (tramp-file-name-user vec)
                 (tramp-file-name-host vec))))
    (concat "/sudo:root@localhost:" tempfile)))

(with-eval-after-load 'dired
  (define-key dired-mode-map [M-return] '+sudo-edit-current-file))
