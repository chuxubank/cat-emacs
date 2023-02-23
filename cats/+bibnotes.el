;; -*- lexical-binding: t; -*-

(defun cat-orb-file ()
  "Return the path to the `org-roam-bibtex' note file"
  (concat cat-org-roam-reference-directory "${citekey}.org"))

(setq bibtex-completion-additional-search-fields '(keywords)
      bibtex-completion-pdf-field "file"
      bibtex-completion-bibliography cat-default-bibliography-files)

(use-package org-roam-bibtex
  :after org-roam
  :custom
  (orb-roam-ref-format 'org-cite)
  :config
  (+add-to-list-multi 'orb-attached-file-extensions "docx" "doc" "epub")
  (+add-to-list-multi 'orb-preformat-keywords "title" "url")
  (setq org-roam-capture-templates
	'(("d" "default" plain "%?"
	   :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
	   :unnarrowed t)
	  ("r" "bibliography reference")
	  ("rd" "Bibliography reference default" plain "%?"
	   :target (file+head "%(cat-orb-file)" "#+title: ${title}")
	   :unnarrowed t)
	  ("rn" "Bibliography reference with org-noter" plain (file "templates/org-noter.org")
	   :target (file "%(cat-orb-file)")
	   :unnarrowed t)
	  ("rl" "Bibliography reference with link" plain "eww:%^{url}"
	   :target (file+head "%(cat-orb-file)" "#+title: ${title}\n#+date: ${date}"))
	  ("rv" "Bibliography reference with video" plain "[[video:%^{url}#]]"
	   :target (file+head "%(cat-orb-file)" "#+title: ${title}\n"))
	  ("rx" "SCSEE XingCe" plain (file "templates/xingce.org")
	   :target (file "%(cat-orb-file)")
	   :unnarrowed t)
	  ("rs" "SCSEE ShenLun" plain (file "templates/shenlun.org")
	   :target (file "%(cat-orb-file)")
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
