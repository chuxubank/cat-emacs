;; -*- lexical-binding: t; -*-

(straight-use-package '(org :type built-in))

(setq org-agenda-files (list cat-org-directory)
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      diary-file (expand-file-name "diary" cat-org-directory)
      ;; org-agenda-include-diary t
      org-auto-align-tags nil
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

(autoload #'org-store-link "ol" nil t)
(define-key global-map (kbd "C-c l") #'org-store-link)

;;; ui
(with-no-warnings
  (custom-declare-face '+org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
  (custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
  (custom-declare-face '+org-todo-onhold  '((t (:inherit (bold warning org-todo)))) "")
  (custom-declare-face '+org-todo-cancel  '((t (:inherit (bold error org-todo)))) ""))
(setq org-todo-keywords
      '((sequence
         "TODO(t)"  ; A task that needs doing & is ready to do
         "PROJ(p)"  ; A project, which usually contains other tasks
         "LOOP(r)"  ; A recurring task
         "STRT(s)"  ; A task that is in progress
         "WAIT(w)"  ; Something external is holding up this task
         "HOLD(h)"  ; This task is paused/on hold because of me
         "IDEA(i)"  ; An unconfirmed and unapproved task or notion
         "|"
         "DONE(d)"  ; Task successfully completed
         "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
        )
      org-todo-keyword-faces
      '(("STRT" . +org-todo-active)
        ("WAIT" . +org-todo-onhold)
        ("HOLD" . +org-todo-onhold)
        ("PROJ" . +org-todo-project)
        ("KILL" . +org-todo-cancel)))

(define-key global-map (kbd "C-c a") #'org-agenda)

;;; capture
(setq org-default-notes-file
      (expand-file-name "notes.org" cat-org-directory)
      org-capture-templates
      '(("t" "Personal todo" entry
         (file "inbox.org")
         "* TODO %?\n%i" :prepend t)
        ("w" "Work todo" entry
         (file+headline "work.org" "Inbox")
         "* TODO %?\n%i" :prepend nil)
        ("b" "Shopping list" entry
         (file "buy.org")
         "* TODO %?\n%i" :prepend t)
        ("p" "Place" entry
         (file+headline "place.org" "Inbox")
         (file "templates/place.org")
         :prepend t
         :empty-lines 1)
        ("n" "Personal notes" entry
         (file+headline "notes.org" "Inbox")
         "* %u %?\n%i\n%a" :prepend t)
        ("j" "Journal" entry
         (file+olp+datetree "journal.org")
         "* %U %?")
        ("c" "ChatGPT" entry
         (file+olp+datetree "ChatGPT.org")
         "* %?")))

(define-key global-map (kbd "C-c c") #'org-capture)

;;; habit
(with-eval-after-load 'org
  (add-to-list 'org-modules 'org-habit))

;;; clock
(setq org-clock-persist 'history
      org-clock-persist-file (expand-file-name "org-clock-save.el" cat-etc-dir)
      org-clock-clocked-in-display 'frame-title)
(with-eval-after-load 'org
  (org-clock-persistence-insinuate))

;;; timer
(setq org-timer-display 'frame-title)

;;; archive
(setq org-archive-location (concat cat-org-directory "/archive.org::* From %s"))

;;; babel
(defun +org-edit-src-eval ()
  "Evaluate the org-mode source block from the org-edit-special buffer and then re-enter the edit buffer."
  (interactive)
  (let ((current-point (point)))
    (org-edit-src-exit)
    (org-ctrl-c-ctrl-c)
    (org-edit-special)
    (goto-char current-point)))
(with-eval-after-load 'org-src
  (define-key org-src-mode-map (kbd "C-c C-c") #'+org-edit-src-eval))
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
