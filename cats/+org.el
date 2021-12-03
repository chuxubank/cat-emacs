;; -*- lexical-binding: t; -*-

(if EMACS28+ nil
  (use-package org
    :defer t))

(straight-use-package '(org :type built-in))

(setq org-agenda-files (list cat-org-directory)
      diary-file (expand-file-name "diary" cat-org-directory)
      ;; org-agenda-include-diary t
      org-id-locations-file (expand-file-name "org-id-locations" cat-etc-dir)
      org-startup-indented t
      org-return-follows-link t
      org-tags-column 0
      org-outline-path-complete-in-steps nil
      org-goto-auto-isearch nil
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
      org-format-latex-header
      "\\documentclass[dvisvgm]{article}
\\usepackage[usenames]{color}
\[PACKAGES]
\[DEFAULT-PACKAGES]
\\pagestyle{empty}             % do not remove
% The settings below are copied from fullpage.sty
\\setlength{\\textwidth}{\\paperwidth}
\\addtolength{\\textwidth}{-3cm}
\\setlength{\\oddsidemargin}{1.5cm}
\\addtolength{\\oddsidemargin}{-2.54cm}
\\setlength{\\evensidemargin}{\\oddsidemargin}
\\setlength{\\textheight}{\\paperheight}
\\addtolength{\\textheight}{-\\headheight}
\\addtolength{\\textheight}{-\\headsep}
\\addtolength{\\textheight}{-\\footskip}
\\addtolength{\\textheight}{-3cm}
\\setlength{\\topmargin}{1.5cm}
\\addtolength{\\topmargin}{-2.54cm}")

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
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (R . t)
     (shell . t)
     (C . t))))
