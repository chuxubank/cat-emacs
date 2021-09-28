(defvar cat-default-bibliography-files '("~/Zotero/My Library.bib"))

(setq reftex-default-bibliography cat-default-bibliography-files
      org-cite-global-bibliography cat-default-bibliography-files)

(use-package bibtex-completion
  :defer t
  :config
  (setq bibtex-completion-additional-search-fields '(keywords)
        bibtex-completion-pdf-field "file"
	bibtex-completion-bibliography cat-default-bibliography-files))

(use-package citeproc
  :defer t)

(use-package embark
  :defer t)

(use-package oc-bibtex-actions
  :ensure bibtex-actions
  :after oc
  :config
  (setq bibtex-actions-at-point-function 'embark-act
	org-cite-insert-processor 'oc-bibtex-actions
        org-cite-follow-processor 'oc-bibtex-actions))

(use-package org-roam-bibtex
  :after org-roam
  :config
  (require 'ox)
  (add-to-list
   'org-roam-capture-templates
   '("r" "bibliography reference" plain
     (file "org-roam-bibtex-template.org")
     :if-new
     (file+head "ref/${citekey}.org" "#+title: ${title}\n")
     :unnarrowed t))
  (org-roam-bibtex-mode +1))

(define-key global-map (kbd "C-c n") #'org-noter)
