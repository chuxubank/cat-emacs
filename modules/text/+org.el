;; -*- lexical-binding: t; -*-

(use-package org
  :cat-font (body
             :modes org-mode
             :faces ((org-document-title title)
                     (org-level-* heading)
                     (org-table table)
                     (org-formula table)
                     (org-column-title table)
                     (org-code code)
                     (org-block code)
                     (org-meta-line code)
                     (org-special-keyword metadata-label)
                     (org-drawer metadata-label)
                     (org-todo metadata-label)
                     (org-done metadata-label)
                     (org-date metadata-value)
                     (org-property-value metadata-value)))
  :vc (org-mode :url "https://code.tecosaur.net/tec/org-mode"
                :lisp-dir "lisp/"
                :rev "dev")
  :delight
  (org-cdlatex-mode (:eval (+with-icon "nf-seti-tex" " ")))
  :commands
  (org-store-link)
  :custom
  (org-directory cat-org-directory)
  (org-agenda-files (list cat-org-directory))
  (org-default-notes-file (expand-file-name "notes.org" cat-org-directory))
  (org-archive-location (concat (expand-file-name "archive/archive.org" cat-org-directory) "::* From %s"))
  (org-auto-align-tags nil)
  (org-startup-indented t)
  (org-startup-with-link-previews t)
  (org-highlight-latex-and-related '(native script entities))
  (org-hide-emphasis-markers t)
  (org-hide-macro-markers t)
  (org-pretty-entities t)
  (org-use-sub-superscripts '{})
  (org-export-with-sub-superscripts '{})
  (org-special-ctrl-a/e t)
  (org-tags-column 0)
  (org-blank-before-new-entry '((heading . t) (plain-list-item . auto)))
  (org-export-backends '(ascii beamer html icalendar latex md odt))
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
  (+org-todo-onhold  ((t (:inherit (bold warning org-todo)))))
  (+org-todo-cancel  ((t (:inherit (bold org-done) :strike-through t))))
  :mode-hydra
  (org-mode
   (:title (+with-icon "nf-custom-orgmode" nil " Org Mode"))
   ("Notes"
    (("i" #'org-id-get-create "id"))
    "Toggle"
    (("tp" org-toggle-pretty-entities "pretty entities" :color red)
     ("te" cat/org-toggle-emphasis "emphasis" :color red)
     ("tm" cat/org-toggle-macro "macro" :color red))))
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
                            "* TODO %?\n%i"
                            :prepend t
                            :empty-lines 1)
                           ("w" "Work todo" entry
                            (file+headline "work.org.gpg" "Inbox")
                            "* TODO %?\n%i"
                            :prepend t
                            :empty-lines 1)
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
                            "* %u %?\n%i\n%a"
                            :prepend t
                            :empty-lines 1)
                           ("j" "Journal" entry
                            (file+olp+datetree "journal.org.gpg")
                            "* %U %?")))
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
  :delight (org-indent-mode
            (:eval (+with-icon "nf-md-format_indent_increase" " "))))

(use-package org-src
  :ensure nil
  :delight (org-src-mode (:eval (+with-icon "nf-fa-code" " "))))

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
  (org-crypt-key (getenv "EMAIL"))
  :config
  (add-to-list 'org-tags-exclude-from-inheritance
               org-crypt-tag-matcher)
  (add-to-list 'org-tag-alist
               (cons org-crypt-tag-matcher ?c))
  (org-crypt-use-before-save-magic))

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
  (org-modern-keyword
   `(("archive" . ,(+with-icon "nf-cod-archive"))
     ("category" . ,(+with-icon "nf-cod-folder"))
     ("columns" . ,(+with-icon "nf-md-view_column"))
     ("constants" . ,(+with-icon "nf-cod-symbol_constant"))
     ("filetags" . ,(+with-icon "nf-fa-tags"))
     ("link" . ,(+with-icon "nf-cod-link"))
     ("priorities" . ,(+with-icon "nf-cod-flame"))
     ("property" . ,(+with-icon "nf-cod-symbol_property"))
     ("setupfile" . ,(+with-icon "nf-cod-file"))
     ("startup" . ,(+with-icon "nf-cod-rocket"))
     ("tags" . ,(+with-icon "nf-fa-hashtag"))
     ("todo" . ,(+with-icon "nf-cod-checklist"))
     ("author" . ,(+with-icon "nf-oct-person"))
     ("creator" . ,(+with-icon "nf-oct-pencil"))
     ("date" . ,(+with-icon "nf-oct-calendar"))
     ("email" . ,(+with-icon "nf-oct-mail"))
     ("language" . ,(+with-icon "nf-fa-language"))
     ("select_tags" . ,(+with-icon "nf-md-tag_plus"))
     ("exclude_tags" . ,(+with-icon "nf-md-tag_minus"))
     ("title" . ,(+with-icon "nf-cod-bookmark"))
     ("export_file_name" . ,(+with-icon "nf-md-file_export"))
     ("options" . ,(+with-icon "nf-cod-settings"))
     ("description" . ,(+with-icon "nf-cod-note"))
     ("latex_class" . ,(concat (+with-icon "nf-seti-tex") " "
                                (+with-icon "nf-cod-file_code")))
     ("latex_class_options" . ,(concat (+with-icon "nf-seti-tex") " "
                                        (+with-icon "nf-cod-file_code") " "
                                        (+with-icon "nf-cod-settings")))
     ("latex_header" . ,(concat (+with-icon "nf-seti-tex") " "
                                 (+with-icon "nf-md-page_layout_header")))
     ("latex" . ,(+with-icon "nf-seti-tex"))
     ("tblfm" . ,(+with-icon "nf-md-math_integral_box"))
     ("name" . ,(+with-icon "nf-md-tag_text"))
     ("call" . ,(+with-icon "nf-cod-terminal"))
     ("include" . ,(+with-icon "nf-oct-cross_reference"))
     ("macro" . ,(+with-icon "nf-fa-cog"))
     ("results" . ,(+with-icon "nf-cod-output" nil ":"))
     (t . t)))
  :mode-hydra
  (org-mode
   ("Mode"
    (("m" org-modern-mode "modern" :color red))))
  :config
  (global-org-modern-mode))

(use-package org-modern-indent
  :pin jcs-elpa
  :hook (org-modern-mode . org-modern-indent-mode))

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t)
  :mode-hydra
  (org-mode
   ("Mode"
    (("a" org-appear-mode "appear" :color red)))))

(defun cat/org-toggle-emphasis ()
  "Toggle org emphasize markers."
  (interactive)
  (setq-local org-hide-emphasis-markers (not org-hide-emphasis-markers))
  (org-restart-font-lock))

(defun cat/org-toggle-macro ()
  "Toggle org macro markers."
  (interactive)
  (setq-local org-hide-macro-markers (not org-hide-macro-markers))
  (org-restart-font-lock))

(defun cat/find-org-files ()
  (interactive)
  (+project-find-file-in-dir cat-org-directory nil t))

(defun cat/preload-org-agenda ()
  "Preload Org agenda files, useful when running as a daemon."
  (cat-benchmark 'beg "preload org agenda files.")
  (require 'org)
  (if (bound-and-true-p org-agenda-files)
      (let ((files (org-agenda-files nil 'ifmode)))
        (org-agenda-prepare-buffers files)
        (cat-benchmark 'end (format "preload %s org agenda files." (length files))))
    (message "Org agenda files not set, skipping preload.")))

(add-hook 'cat-idle-preload-hook #'cat/preload-org-agenda)
