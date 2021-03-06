;; -*- lexical-binding: t; -*-

(use-package org-roam
  :defer t
  :init
  (setq org-roam-v2-ack t
	org-roam-directory cat-org-roam-directory
        org-roam-db-location (expand-file-name "org-roam.db" cat-etc-dir)
	org-roam-completion-everywhere t
	org-roam-node-display-template "${tags:10} ${title:*}"
	org-roam-mode-section-functions
	(list #'org-roam-backlinks-section
              #'org-roam-reflinks-section
              #'org-roam-unlinked-references-section))
  :custom
  (org-roam-dailies-directory cat-org-roam-dailies-directory)
  :config
  (org-roam-db-autosync-mode)
  (org-roam-update-org-id-locations)
  (require 'org-roam-protocol)
  (setq org-roam-capture-ref-templates
	'(("r" "Protocol Capture Reference"
	   plain "${body}%?" :target
	   (file+head "capture/${Input file name}.org" "#+title: ${title}\n")
	   :unnarrowed t)
	  ("c" "Course"
	   plain (file "templates/course.org") :target
	   (file "course/${SOURCE|cmu|mit}/${COURSE-ID}.org")
	   :unnarrowed t))))

(use-package org-roam-dailies
  :ensure org-roam
  :bind-keymap
  ("C-c d" . org-roam-dailies-map))

(use-package org-roam-ui
  :defer t
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t
	org-roam-ui-ref-title-template
	"%^{author-abbrev} (%^{date}) %^{title}"))

;;; bibtex
(use-package bibtex-completion
  :defer t
  :config
  (setq bibtex-completion-additional-search-fields '(keywords)
        bibtex-completion-pdf-field "file"
	bibtex-completion-bibliography cat-default-bibliography-files))

(use-package org-roam-bibtex
  :after org-roam
  :config
  (require 'ox)
  (setq orb-roam-ref-format 'org-cite
	orb-note-actions-interface #'citar-run-default-action
	orb-attached-file-extensions '("pdf" "docx" "doc" "epub")
	orb-preformat-keywords
	'("title" "url" "citekey" "entry-type" "date" "pdf?" "note?" "file" "author" "editor" "author-abbrev" "editor-abbrev" "author-or-editor-abbrev")
	org-roam-capture-templates
	'(("d" "default" plain "%?" :target
	   (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
	   :unnarrowed t)
	  ("b" "bibliography")
	  ("bd" "Bibliography reference default"
	   plain "%?" :target
	   (file+head "reference/${citekey}.org" "#+title: ${title}")
	   :unnarrowed t)
	  ("bn" "Bibliography reference with org-noter"
	   plain (file "templates/org-noter.org") :target
	   (file "reference/${citekey}.org")
	   :unnarrowed t)
	  ("bl" "Bibliography reference with link"
	   plain "eww:%^{url}" :target
	   (file+head "reference/${citekey}.org" "#+title: ${title}\n#+date: ${date}"))
	  ("bv" "Bibliography reference with video"
	   plain "[[video:%^{url}#]]" :target
	   (file+head "reference/${citekey}.org" "#+title: ${title}\n"))
	  ("bx" "SCSEE XingCe"
	   plain (file "templates/xingce.org") :target
	   (file "reference/${citekey}.org")
	   :unnarrowed t)
	  ("bs" "SCSEE ShenLun"
	   plain (file "templates/shenlun.org") :target
	   (file "reference/${citekey}.org")
	   :unnarrowed t)))
  (org-roam-bibtex-mode +1))

(defvar cat-org-roam-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" #'org-roam-capture)
    (define-key map "d" (cons "org-roam-dailies"
			      #'(lambda ()
				  (interactive)
				  (use-package-autoload-keymap
				   'org-roam-dailies-map
				   'org-roam-dailies
				   nil))))
    (define-key map "f" #'org-roam-node-find)
    (define-key map "F" #'org-roam-ref-find)
    (define-key map "g" #'org-roam-graph)
    (define-key map "i" #'org-roam-node-insert)
    (define-key map "R" #'org-roam-buffer-display-dedicated)
    (define-key map "u" #'org-roam-ui-mode)
    map)
  "Keymap for org-roam commands.")
