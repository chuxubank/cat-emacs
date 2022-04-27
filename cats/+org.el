;; -*- lexical-binding: t; -*-

(if EMACS28+
    nil
  (use-package org
    :defer t))

(straight-use-package '(org :type built-in))

(setq org-agenda-files (list cat-org-directory)
      diary-file (expand-file-name "diary" cat-org-directory)
      ;; org-agenda-include-diary t
      org-id-locations-file (expand-file-name "org-id-locations" cat-etc-dir)
      org-startup-indented t
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

;;; latex
(setq org-latex-compiler "xelatex"
      org-preview-latex-image-directory (concat cat-cache-dir "org-latex/")
      org-latex-packages-alist
      '(("" "ctex" t ("xelatex"))
	("" "booktabs" nil)
	("" "enumitem" nil)
	("" "fontspec" nil)
	("" "svg" nil)
	("" "pgfplots" t)
	("left=2.5cm, right=2.5cm, top=2cm, bottom=2cm" "geometry" nil)))
(with-eval-after-load 'org
  (setq org-format-latex-header
	(replace-regexp-in-string (regexp-quote "\\documentclass")
				  "\\documentclass[dvisvgm]"
				  org-format-latex-header t t)))

;;; preview
(setq org-preview-latex-default-process 'dvisvgm
      org-preview-latex-process-alist
      '((dvisvgm :programs ("xelatex" "dvisvgm")
                 :description "xdv > svg"
                 :message "you need to install the programs: xelatex and dvisvgm."
                 :image-input-type "xdv"
                 :image-output-type "svg"
                 :image-size-adjust (2.5 . 1.5)
                 :latex-compiler ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
                 :image-converter ("dvisvgm %f -n -b min -c %S -o %O"))
        (imagemagick :programs("xelatex" "convert")
                     :description "pdf > png"
                     :message "you need to install the programs: xelatex and imagemagick."
                     :image-input-type "pdf"
                     :image-output-type "png"
                     :image-size-adjust (1.0 . 1.0)
                     :latex-compiler ("xelatex -interaction nonstopmode -output-directory %o %f")
                     :image-converter ("convert -density %D -trim -antialias %f -quality 100 %O"))))

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
        (sequence
         "[ ](T)"   ; A task that needs doing
         "[-](S)"   ; Task is in progress
         "[?](W)"   ; Task is being held up or paused
         "|"
         "[X](D)")  ; Task was completed
        (sequence
         "|"
         "OKAY(o)"
         "YES(y)"
         "NO(n)"))
      org-todo-keyword-faces
      '(("[-]"  . +org-todo-active)
        ("STRT" . +org-todo-active)
        ("[?]"  . +org-todo-onhold)
        ("WAIT" . +org-todo-onhold)
        ("HOLD" . +org-todo-onhold)
        ("PROJ" . +org-todo-project)
        ("NO"   . +org-todo-cancel)
        ("KILL" . +org-todo-cancel)))

(define-key global-map (kbd "C-c a") #'org-agenda)

;;; capture
(setq org-default-notes-file
      (expand-file-name "notes.org" cat-org-directory)
      org-capture-templates
      '(("t" "Personal todo" entry
         (file "inbox.org")
         "* [ ] %?\n%i" :prepend t)
	("b" "Shopping list" entry
         (file "buy.org")
         "* TODO %?\n%i" :prepend t)
        ("n" "Personal notes" entry
         (file+headline "notes.org" "Inbox")
         "* %u %?\n%i\n%a" :prepend t)
        ("j" "Journal" entry
         (file+olp+datetree "journal.org")
         "* %U %?")))

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

;;; column
(with-eval-after-load 'org-colview
  (defun org-columns--display-here (columns &optional dateline)
    "Overlay the current line with column display.
COLUMNS is an alist (SPEC VALUE DISPLAYED).  Optional argument
DATELINE is non-nil when the face used should be
`org-agenda-column-dateline'."
    (when (ignore-errors (require 'face-remap))
      (setq org-columns-header-line-remap
	    (face-remap-add-relative 'header-line '(:inherit default))))
    (save-excursion
      (beginning-of-line)
      (let* ((level-face (and (looking-at "\\(\\**\\)\\(\\* \\)")
			      (org-get-level-face 2)))
	     (ref-face (or level-face
			   (and (eq major-mode 'org-agenda-mode)
				(org-get-at-bol 'face))
			   'default))
	     (color (list :foreground (face-attribute ref-face :foreground)))
	     (font (list :family cat-mono-font))
	     (face (list color font 'org-column ref-face))
	     (face1 (list color font 'org-agenda-column-dateline ref-face)))
	;; Each column is an overlay on top of a character.  So there has
	;; to be at least as many characters available on the line as
	;; columns to display.
	(let ((columns (length org-columns-current-fmt-compiled))
	      (chars (- (line-end-position) (line-beginning-position))))
	  (when (> columns chars)
	    (save-excursion
	      (end-of-line)
	      (let ((inhibit-read-only t))
		(insert (make-string (- columns chars) ?\s))))))
	;; Display columns.  Create and install the overlay for the
	;; current column on the next character.
	(let ((i 0)
	      (last (1- (length columns))))
	  (dolist (column columns)
	    (pcase column
	      (`(,spec ,original ,value)
	       (let* ((property (car spec))
		      (width (aref org-columns-current-maxwidths i))
		      (fmt (format (if (= i last) "%%-%d.%ds |"
				     "%%-%d.%ds | ")
				   width width))
		      (ov (org-columns--new-overlay
			   (point) (1+ (point))
			   (org-columns--overlay-text
			    value fmt width property original)
			   (if dateline face1 face))))
		 (overlay-put ov 'keymap org-columns-map)
		 (overlay-put ov 'org-columns-key property)
		 (overlay-put ov 'org-columns-value original)
		 (overlay-put ov 'org-columns-value-modified value)
		 (overlay-put ov 'org-columns-format fmt)
		 (overlay-put ov 'line-prefix "")
		 (overlay-put ov 'wrap-prefix "")
		 (forward-char))))
	    (cl-incf i)))
	;; Make the rest of the line disappear.
	(let ((ov (org-columns--new-overlay (point) (line-end-position))))
	  (overlay-put ov 'invisible t)
	  (overlay-put ov 'keymap org-columns-map)
	  (overlay-put ov 'line-prefix "")
	  (overlay-put ov 'wrap-prefix ""))
	(let ((ov (make-overlay (1- (line-end-position))
				(line-beginning-position 2))))
	  (overlay-put ov 'keymap org-columns-map)
	  (push ov org-columns-overlays))
	(with-silent-modifications
	  (let ((inhibit-read-only t))
	    (put-text-property
	     (line-end-position 0)
	     (line-beginning-position 2)
	     'read-only
	     (substitute-command-keys
	      "Type \\<org-columns-map>`\\[org-columns-edit-value]' \
to edit property"))))))))

;;; babel
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (R . t)
     (shell . t)
     (C . t)
     (plantuml . t))))

;;; font
(defun cat-setup-org-font ()
  (set-face-font 'org-table cat-mono-font)
  (set-face-font 'org-column-title cat-mono-font))

(add-hook 'org-mode-hook #'cat-setup-org-font)
