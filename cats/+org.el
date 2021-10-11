(defvar cat-org-directory "~/org")

(straight-use-package '(org :type built-in))

(if EMACS28+ nil
  (use-package org
    :defer t))

(straight-use-package '(org :type built-in))

(setq org-agenda-files (list cat-org-directory)
      org-id-locations-file (expand-file-name "org-id-locations" cat-etc-dir)
      org-startup-indented t
      org-return-follows-link t
      org-tags-column 0
      org-outline-path-complete-in-steps nil
      org-refile-use-outline-path 'file
      org-refile-targets
      '((nil :maxlevel . 10)
	(org-agenda-files :maxlevel . 3))
      org-list-allow-alphabetical t
      ;; Sub-lists should have different bullets
      org-list-demote-modify-bullet
      '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a."))
      org-image-actual-width 500
      org-display-remote-inline-images 'cache)

;;; link
(setq org-link-abbrev-alist
      '(("wiki-zh" . "https://zh.wikipedia.org/wiki/%h")
	("wiki-en" . "https://en.wikipedia.org/wiki/%s")
	("github" . "https://github.com/%s")
	("bili". "https://bilibili.com/bangumi/media/%s")
	("coursera". "https://www.coursera.org/%s")))

(with-eval-after-load 'org
  (setq org-directory cat-org-directory)
  (when IS-LINUX
    (add-to-list 'org-file-apps '("\\.x?html\\'" . "firefox %s")))
  (define-key org-mode-map (kbd "M-n") #'org-next-link)
  (define-key org-mode-map (kbd "M-p") #'org-previous-link)
  (define-key org-mode-map (kbd "C-c C-x l") #'org-toggle-link-display))

(define-key global-map (kbd "C-c l") #'org-store-link)

;;; latex
(setq org-latex-compiler "xelatex"
      org-preview-latex-image-directory (concat cat-cache-dir "org-latex/")
      org-latex-packages-alist
      '(("" "ctex" t ("xelatex"))
	("" "booktabs" nil)
	("" "enumitem" nil)
	("" "fontspec" nil)
	("" "pgfplots" t)
	("left=2.5cm, right=2.5cm, top=2cm, bottom=2cm" "geometry" nil))
      org-format-latex-options
      '(:foreground default
       		    :background default
		    :scale 1.5
       		    :html-foreground "Black"
       		    :html-background "Transparent"
       		    :html-scale 1.0
       		    :matchers
       		    ("begin" "$1" "$" "$$" "\\(" "\\[")))

;;; preview
(setq org-preview-latex-default-process 'dvisvgm
      org-preview-latex-process-alist
      '((dvisvgm :programs ("xelatex" "dvisvgm")
                 :description "xdv > svg"
                 :message "you need to install the programs: xelatex and dvisvgm."
                 :image-input-type "xdv"
                 :image-output-type "svg"
                 :image-size-adjust (1.7 . 1.5)
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
         (file+headline "inbox.org" "Inbox")
         "* [ ] %?\n%i\n%a" :prepend t)
        ("n" "Personal notes" entry
         (file+headline "notes.org" "Inbox")
         "* %u %?\n%i\n%a" :prepend t)
        ("j" "Journal" entry
         (file+olp+datetree "journal.org")
         "* %U %?\n%i\n%a" :prepend t)))

(define-key global-map (kbd "C-c c") #'org-capture)

;;; habit
(with-eval-after-load 'org
  (add-to-list 'org-modules 'org-habit))

;;; company
(add-hook 'org-mode-hook (lambda () (add-to-list 'company-backends #'org-keyword-backend)))
