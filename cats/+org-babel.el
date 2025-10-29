;; -*- lexical-binding: t; -*-

(defun +org-redisplay-inline-images-in-babel-result-h ()
  (unless (or
           ;; ...but not while Emacs is exporting an org buffer (where
           ;; `org-display-inline-images' can be awfully slow).
           (bound-and-true-p org-export-current-backend)
           ;; ...and not while tangling org buffers (which happens in a temp
           ;; buffer where `buffer-file-name' is nil).
           (string-match-p "^ \\*temp" (buffer-name)))
    (save-excursion
      (when-let ((beg (org-babel-where-is-src-block-result))
                 (end (progn (goto-char beg) (forward-line) (org-babel-result-end))))
        (org-display-inline-images nil nil (min beg end) (max beg end))))))

(use-package ob-core
  :ensure nil
  :hook
  (org-babel-after-execute . #'+org-redisplay-inline-images-in-babel-result-h))

(use-package ob-lob
  :ensure nil
  :config
  (org-babel-lob-ingest (expand-file-name "library-of-babel.org" user-emacs-directory)))

(use-package ob-async
  :vc (:url "https://github.com/ezchi/ob-async"
            :rev "develop")
  :demand t
  :after org)
