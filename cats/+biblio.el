(defvar cat-default-bibliography-files '("~/Zotero/My Library.bib"))

(setq reftex-default-bibliography cat-default-bibliography-files)

(use-package bibtex-completion
  :defer t
  :config
  (setq bibtex-completion-additional-search-fields '(keywords)
        bibtex-completion-pdf-field "file"
	bibtex-completion-bibliography cat-default-bibliography-files))

(use-package bibtex-actions
  :bind (("C-c b" . bibtex-actions-insert-citation)
         :map minibuffer-local-map
         ("M-b" . bibtex-actions-insert-preset)))

(use-package org-roam-bibtex
  :after org-roam
  :config
  (add-to-list
   'org-roam-capture-templates
   '("r" "bibliography reference" plain "%?"
     :if-new
     (file+head "ref/${citekey}.org" "#+title: ${title}\n")
     :unnarrowed t)))
