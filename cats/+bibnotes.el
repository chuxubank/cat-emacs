;; -*- lexical-binding: t; -*-

(setq bibtex-completion-additional-search-fields '(keywords)
      bibtex-completion-pdf-field "file"
      bibtex-completion-bibliography cat-default-bibliography-files)

(use-package org-roam-bibtex
  :after org-roam
  :config
  (require 'ox)
  (setq orb-roam-ref-format 'org-cite
	orb-attached-file-extensions '("pdf" "docx" "doc" "epub")
	orb-preformat-keywords
	'("title" "url" "citekey" "entry-type" "date" "pdf?" "note?" "file" "author" "editor" "author-abbrev" "editor-abbrev" "author-or-editor-abbrev")
	org-roam-capture-templates
	'(("d" "default" plain "%?"
	   :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
	   :unnarrowed t)
	  ("b" "bibliography")
	  ("bd" "Bibliography reference default" plain "%?"
	   :target (file+head "reference/${citekey}.org" "#+title: ${title}")
	   :unnarrowed t)
	  ("bn" "Bibliography reference with org-noter" plain (file "templates/org-noter.org")
	   :target (file "reference/${citekey}.org")
	   :unnarrowed t)
	  ("bl" "Bibliography reference with link" plain "eww:%^{url}"
	   :target (file+head "reference/${citekey}.org" "#+title: ${title}\n#+date: ${date}"))
	  ("bv" "Bibliography reference with video" plain "[[video:%^{url}#]]"
	   :target (file+head "reference/${citekey}.org" "#+title: ${title}\n"))
	  ("bx" "SCSEE XingCe" plain (file "templates/xingce.org")
	   :target (file "reference/${citekey}.org")
	   :unnarrowed t)
	  ("bs" "SCSEE ShenLun" plain (file "templates/shenlun.org")
	   :target (file "reference/${citekey}.org")
	   :unnarrowed t)))
  (org-roam-bibtex-mode +1))

(use-package citar-org-roam
  :after citar org-roam-bibtex
  :custom
  (citar-org-roam-capture-template-key "bn")
  (citar-org-roam-subdir cat-org-roam-reference-directory)
  (citar-notes-paths (list (concat cat-org-roam-directory cat-org-roam-reference-directory)))
  :config
  (citar-register-notes-source
   'orb-citar-source (list :name "Org-Roam BibTex Notes"
			   :category 'org-roam-node
			   :items #'citar-org-roam--get-candidates
			   :hasitems #'citar-org-roam-has-notes
			   :open #'citar-org-roam-open-note
			   :create #'orb-citar-edit-note
			   :annotate #'citar-org-roam--annotate))
  (setq citar-notes-source 'orb-citar-source))
