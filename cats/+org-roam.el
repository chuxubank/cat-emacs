;; -*- lexical-binding: t; -*-

(use-package org-roam
  :defer t
  :init
  (setq org-roam-v2-ack t
	org-roam-directory cat-org-roam-directory
        org-roam-db-location (expand-file-name "org-roam.db" cat-etc-dir)
	org-roam-completion-everywhere t
	org-roam-mode-section-functions
	(list #'org-roam-backlinks-section
              #'org-roam-reflinks-section
              #'org-roam-unlinked-references-section))
  :bind (("C-c r r" . #'org-roam-buffer-toggle)
	 ("C-c r R" . #'org-roam-buffer-display-dedicated)
	 ("C-c r f" . #'org-roam-node-find)
	 ("C-c r F" . #'org-roam-ref-find)
         ("C-c r g" . #'org-roam-graph)
         ("C-c r i" . #'org-roam-node-insert)
         ("C-c r c" . #'org-roam-capture))
  :bind-keymap
  ("C-c r d" . org-roam-dailies-map)
  :custom
  (org-roam-dailies-directory cat-org-roam-dailies-directory)
  :config
  (org-roam-db-autosync-mode)
  (org-roam-update-org-id-locations)
  (require 'org-roam-protocol)
  (setq org-roam-capture-ref-templates
	'(("r" "Protocol Capture Reference" plain "${body}%?"
	   :target (file+head "capture/${slug}.org" "#+title: ${title}\n")
	   :unnarrowed t))
	org-roam-node-display-template
	"${tags:10} ${title:*}"))

(straight-use-package '(org-roam :type built-in))

(use-package org-roam-ui
  :straight (org-roam-ui
	     :host github
	     :repo "org-roam/org-roam-ui"
	     :branch "main"
	     :files ("*.el" "out"))
  :after org-roam
  :bind ("C-c r u" . #'org-roam-ui-mode)
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
(straight-use-package '(bibtex-completion :type built-in))

(straight-use-package '(org-ref :type built-in))
(use-package org-roam-bibtex
  :straight t
  :after org-roam
  :config
  (setq orb-roam-ref-format 'org-cite
	orb-note-actions-interface #'citar-run-default-action
	orb-file-field-extensions '("pdf" "docx" "doc" "epub")
	orb-preformat-keywords
	'("title" "url" "citekey" "entry-type" "date" "pdf?" "note?" "file" "author" "editor" "author-abbrev" "editor-abbrev" "author-or-editor-abbrev")
	org-roam-capture-templates
	'(("d" "default" plain "%?" :target
	   (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
	   :unnarrowed t)
	  ("n" "Bibliography reference with org-noter"
	   plain (file "templates/org-noter.org") :target
	   (file+head "reference/${citekey}.org" "#+title: ${title}")
	   :unnarrowed t)
	  ("l" "Bibliography reference with link"
	   plain "eww:%^{url}" :target
	   (file+head "reference/${citekey}.org" "#+title: ${title}\n#+date: ${date}"))))
  (org-roam-bibtex-mode +1))
