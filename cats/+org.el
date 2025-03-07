;; -*- lexical-binding: t; -*-

(use-package org
  :vc (org-mode :url "https://code.tecosaur.net/tec/org-mode"
                :lisp-dir "lisp/"
                :make "compile autoloads")
  :delight
  (org-cdlatex-mode " ")
  :commands
  (org-store-link)
  :custom
  (org-directory cat-org-directory)
  (org-agenda-files (list cat-org-directory))
  (org-default-notes-file (expand-file-name "notes.org" cat-org-directory))
  (org-archive-location (concat cat-org-directory "archive.org::* From %s"))
  (org-auto-align-tags nil)
  (org-startup-indented t)
  (org-startup-with-link-previews t)
  (org-special-ctrl-a/e t)
  (org-tags-column 0)
  (org-export-backends '(ascii beamer html icalendar latex md odt))
  (org-highlight-latex-and-related '(native latex script entities))
  (org-image-actual-width 500)
  (org-display-remote-inline-images 'cache)
  (org-insert-heading-respect-content t)
  (org-todo-keywords '((sequence
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
                       ))
  (org-todo-keyword-faces '(("STRT" . +org-todo-active)
                            ("WAIT" . +org-todo-onhold)
                            ("HOLD" . +org-todo-onhold)
                            ("PROJ" . +org-todo-project)
                            ("KILL" . +org-todo-cancel)))
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies nil)
  :custom-face
  (+org-todo-active  ((t (:inherit (bold font-lock-constant-face org-todo)))))
  (+org-todo-project ((t (:inherit (bold font-lock-doc-face org-todo)))))
  (+org-todo-onhold ((t (:inherit (bold warning org-todo)))))
  (+org-todo-cancel ((t (:inherit (bold org-done) :strike-through t))))
  :mode-hydra
  (org-mode
   (:title (+with-icon "nf-custom-orgmode" "Org Mode"))
   ("Notes"
    (("i" #'org-id-get-create "id")
     ("r" org-roam-hydra/body "roam")
     ("n" org-noter "noter")
     ("v" org-media-note-hydra/body "media"))
    "SRS"
    (("a" org-anki-hydra/body "anki")
     ("d" org-drill-hydra/body "drill"))
    "Jira"
    (("j" org-jira-todo-to-jira "transform"))
    "Display"
    (("m" org-modern-mode "modern mode")
     ("b" org-link-beautify-mode "link beautify")
     ("l" org-toggle-link-display "link display"))
    "Plugin"
    (("c" org-cliplink "cliplink")
     ("e" org-edna-edit "edna edit")
     ("h" consult-org-heading "headings"))))
  :config
  (when IS-LINUX
    (add-to-list 'org-file-apps '("\\.x?html\\'" . "firefox %s")))
  (org-clock-persistence-insinuate)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((awk . t)
     (C . t)
     (emacs-lisp . t)
     (plantuml . t)
     (python . t)
     (R . t)
     (shell . t)))
  (add-to-list 'org-modules 'org-habit)
  (require 'server)
  (require 'org-protocol))

(use-package org-goto
  :ensure nil
  :custom
  (org-goto-auto-isearch nil))

(use-package org-capture
  :ensure nil
  :custom
  (org-capture-templates '(("t" "Personal todo" entry
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
  :config
  (add-hook 'org-capture-after-finalize-hook #'org-save-all-org-buffers))

(use-package org-refile
  :ensure nil
  :custom
  (org-outline-path-complete-in-steps nil)
  (org-refile-use-outline-path 'file)
  (org-refile-targets '((nil :maxlevel . 10)
                        (org-agenda-files :maxlevel . 3))))

(use-package org-habit
  :ensure nil
  :custom
  (org-habit-graph-column 60))

(use-package org-list
  :ensure nil
  :custom
  (org-list-allow-alphabetical t)
  (org-list-demote-modify-bullet '(("+" . "-")
                                   ("-" . "+")
                                   ("*" . "+")
                                   ("1." . "a."))))

(use-package org-indent
  :ensure nil
  :delight  " 󰉶")

(use-package org-src
  :ensure nil
  :delight " ")

(use-package org-keys
  :ensure nil
  :custom
  (org-return-follows-link t))

(use-package org-clock
  :ensure nil
  :custom
  (org-clock-persist 'history)
  (org-clock-clocked-in-display 'frame-title))

(use-package org-timer
  :ensure nil
  :custom
  (org-timer-display 'frame-title))

(use-package org-crypt
  :ensure nil
  :demand t
  :after org
  :bind
  (:map org-mode-map
        ("C-c C-/" . org-decrypt-entries))
  :custom
  (org-crypt-disable-auto-save 'encrypt)
  :config
  (add-to-list 'org-tags-exclude-from-inheritance
               org-crypt-tag-matcher)
  (add-to-list 'org-tag-alist
               (cons org-crypt-tag-matcher ?c))
  (org-crypt-use-before-save-magic))

(use-package ol
  :demand t
  :after org
  :ensure nil
  :bind
  (:map org-mode-map
        ("M-n" . org-next-link)
        ("M-p" . org-previous-link))
  :custom
  (org-link-abbrev-alist '(("wiki-zh" . "https://zh.wikipedia.org/wiki/%h")
                           ("wiki-en" . "https://en.wikipedia.org/wiki/%s")
                           ("github" . "https://github.com/%s")
                           ("google" . "https://goo.gle/%s")
                           ("bitbucket" . "https://bitbucket.org/%s")
                           ("bili". "https://bilibili.com/video/%s")
                           ("coursera". "https://www.coursera.org/%s"))))

(use-package ob-core
  :ensure nil
  :init
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
  (add-hook 'org-babel-after-execute-hook #'+org-redisplay-inline-images-in-babel-result-h))

(use-package ob-lob
  :ensure nil
  :config
  (org-babel-lob-ingest (expand-file-name "library-of-babel.org" user-emacs-directory)))

(use-package org-modern
  :demand
  :after org
  :custom
  (org-modern-table nil)
  (org-modern-hide-stars nil)
  (org-modern-todo-faces '(("STRT" :inverse-video t :inherit +org-todo-active)
                           ("WAIT" :inverse-video t :inherit +org-todo-onhold)
                           ("HOLD" :inverse-video t :inherit +org-todo-onhold)
                           ("PROJ" :inverse-video t :inherit +org-todo-project)
                           ("KILL" :inverse-video t :inherit +org-todo-cancel)))
  (org-modern-keyword `(;; (info "(org) In-Buffer Settings")
                        ("archive" . "")
                        ("category" . "")
                        ("columns" . "󰕭")
                        ("constants" . "")
                        ("filetags" . "")
                        ("link" . "")
                        ("priorities" . "")
                        ("property" . "")
                        ("setupfile" . "")
                        ("startup" . "")
                        ("tags" . "")
                        ("todo" . "")
                        ;; (info "(org) Export Settings")
                        ("author" . "")
                        ("creator" . "")
                        ("date" . "")
                        ("email" . "")
                        ("language" . "")
                        ("select_tags" . "󰜢")
                        ("exclude_tags" . "󰤐")
                        ("title" . "")
                        ("export_file_name" . "󰈝")
                        ("options" . "")
                        ;; (info "(org) LaTeX specific export settings")
                        ("description" . "")
                        ("latex_class" . " ")
                        ("latex_class_options" . "  ")
                        ("latex_header" . " 󰛼")
                        ("latex" . "")
                        ;; inpage
                        ("tblfm" . "󰿉")
                        ("name" . "󱈤")
                        ("call" . "")
                        ("include" . ,(+with-icon "nf-oct-cross_reference"))
                        ("macro" . "")
                        (t . t)))
  :config
  (global-org-modern-mode))

(use-package org-modern-indent
  :pin jcs-elpa
  :hook (org-mode . org-modern-indent-mode))

(defun +find-org-files ()
  (interactive)
  (+project-find-file-in-dir cat-org-directory nil t))
