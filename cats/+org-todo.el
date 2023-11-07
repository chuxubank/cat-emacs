;; -*- lexical-binding: t; -*-

(setq org-agenda-files (list cat-org-directory)
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      diary-file (expand-file-name "diary" cat-org-directory)
      org-agenda-include-diary t
      org-archive-location (concat cat-org-directory "archive.org::* From %s"))

(with-no-warnings
  (custom-declare-face '+org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
  (custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
  (custom-declare-face '+org-todo-onhold  '((t (:inherit (bold warning org-todo)))) "")
  (custom-declare-face '+org-todo-cancel  '((t (:inherit (bold org-done) :strike-through t))) ""))
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

;;; habit
(with-eval-after-load 'org
  (add-to-list 'org-modules 'org-habit))

(use-package org-agenda-count
  :vc (:url "https://github.com/sid-kurias/org-agenda-count" :rev :newest)
  :after org
  :custom
  (org-agenda-custom-commands
   '(("n" "Agenda and all TODOs"
      ((agenda
        ""
        ((org-agenda-span 'day)
         (org-agenda-include-diary nil)
         (org-agenda-overriding-header
          (format "Agenda [%s]" (org-agenda-count)))))
       (alltodo
        ""
        ((org-agenda-overriding-header
          (format "All TODOs [%s]" (org-agenda-count "alltodo"))))))))))
