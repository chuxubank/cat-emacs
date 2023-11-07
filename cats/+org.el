;; -*- lexical-binding: t; -*-

(setq org-auto-align-tags nil
      org-hide-emphasis-markers t
      org-id-locations-file (expand-file-name "org-id-locations" cat-etc-dir)
      org-startup-indented t
      org-special-ctrl-a/e t
      org-tags-column 0
      org-outline-path-complete-in-steps nil
      ;; org-goto
      org-goto-auto-isearch nil
      ;; org-refile
      org-refile-use-outline-path 'file
      org-refile-targets
      '((nil :maxlevel . 10)
        (org-agenda-files :maxlevel . 3))
      ;; org-list
      org-list-allow-alphabetical t
      org-list-demote-modify-bullet
      '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a."))
      ;; org-export
      org-export-backends
      '(ascii beamer html icalendar latex md odt))

;;; image
(setq org-image-actual-width 500
      org-display-remote-inline-images 'cache)

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

(add-hook 'org-babel-after-execute-hook #'+org-redisplay-inline-images-in-babel-result-h)

;;; link
(setq org-return-follows-link t
      org-link-abbrev-alist
      '(("wiki-zh" . "https://zh.wikipedia.org/wiki/%h")
        ("wiki-en" . "https://en.wikipedia.org/wiki/%s")
        ("github" . "https://github.com/%s")
        ("google" . "https://goo.gle/%s")
        ("bitbucket" . "https://bitbucket.org/%s")
        ("bili". "https://bilibili.com/video/%s")
        ("coursera". "https://www.coursera.org/%s")))

(with-eval-after-load 'org
  (setq org-directory cat-org-directory)
  (require 'server)
  (require 'org-protocol)
  (when IS-LINUX
    (add-to-list 'org-file-apps '("\\.x?html\\'" . "firefox %s")))
  (define-key org-mode-map (kbd "M-n") #'org-next-link)
  (define-key org-mode-map (kbd "M-p") #'org-previous-link)
  (define-key org-mode-map (kbd "C-c C-x l") #'org-toggle-link-display))

;;; clock
(setq org-clock-persist 'history
      org-clock-persist-file (expand-file-name "org-clock-save.el" cat-etc-dir)
      org-clock-clocked-in-display 'frame-title)
(with-eval-after-load 'org
  (org-clock-persistence-insinuate))

;;; timer
(setq org-timer-display 'frame-title)

;;; babel
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (R . t)
     (shell . t)
     (C . t)
     (plantuml . t)
     (awk . t))))
(with-eval-after-load 'ob-lob
  (org-babel-lob-ingest (expand-file-name "library-of-babel.org" user-emacs-directory)))

;;; font
(defun cat-setup-org-font ()
  (set-face-font 'org-table cat-mono-font)
  (set-face-font 'org-column-title cat-mono-font))

(add-hook 'org-mode-hook #'cat-setup-org-font)

;;; refresh
(defun +org-buffers-refresh ()
  "Save and revert all org buffers without confirm."
  (interactive)
  (org-save-all-org-buffers)
  (+no-confirm #'org-revert-all-org-buffers)
  (org-element-update-syntax))

(autoload 'org-store-link "ol"
  "Jump to Dired buffer corresponding to current buffer." t)
