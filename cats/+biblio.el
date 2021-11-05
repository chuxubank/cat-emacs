(defvar cat-default-bibliography-files '("~/Zotero/My Library.bib"))
(defvar cat-default-csl-styles-dir "~/Zotero/styles")

(setq reftex-default-bibliography cat-default-bibliography-files
      org-cite-global-bibliography cat-default-bibliography-files
      org-cite-csl-styles-dir cat-default-csl-styles-dir)

(use-package bibtex-completion
  :defer t
  :config
  (setq bibtex-completion-additional-search-fields '(keywords)
        bibtex-completion-pdf-field "file"
	bibtex-completion-bibliography cat-default-bibliography-files))

(use-package citeproc
  :defer t)

(use-package citar-org
  :ensure citar
  :after oc
  :config
  (setq citar-at-point-function 'embark-act
	citar-bibliography cat-default-bibliography-files
	citar-file-note-org-include '(org-id org-roam-ref)
	org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar)
  (citar-filenotify-setup '(LaTeX-mode-hook org-mode-hook))
  (define-key citar-org-citation-map (kbd "C-p") nil)
  (define-key citar-org-citation-map (kbd "C-c C-l") #'citar-org-update-pre-suffix))

(use-package org-roam-bibtex
  :after org-roam
  :config
  (setq orb-citekey-format "@%s"
	orb-file-field-extensions '("pdf" "docx" "doc" "epub")
	citar-notes-paths (list cat-org-roam-references-dir))
  (require 'ox)
  (add-to-list 'orb-preformat-keywords "title")
  (add-to-list
   'org-roam-capture-templates
   '("r" "bibliography reference" plain
     (file "org-roam-bibtex-template.org")
     :target
     (file+head "ref/${citekey}.org" "#+title: ${title}\n")
     :unnarrowed t))
  (org-roam-bibtex-mode +1)
  (setq citar-open-note-function 'orb-bibtex-actions-edit-note))
