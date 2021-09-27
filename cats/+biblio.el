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

(use-package org-ref
  :after org
  (setq org-ref-completion-library #'org-ref-reftex)
  :config
  (setq org-ref-default-bibliography cat-default-bibliography-files
	org-ref-open-pdf-function 'org-ref-get-pdf-filename-helm-bibtex))

(use-package org-roam-bibtex
  :after org-roam
  :config
  (setq org-roam-capture-templates
	'(("d" "default" plain "%?"
	   :target
	   (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
	   :unnarrowed t)
	  ("r" "bibliography reference" plain "%?"
	   :if-new
	   (file+head "ref/${citekey}.org" "#+title: ${title}\n")
	   :unnarrowed t)
	  ("b" "Bibliography note" plain
           "%?
- keywords :: %^{keywords}
- related ::
* %^{title}
:PROPERTIES:
:Custom_ID: %^{citekey}
:URL: %^{url}
:AUTHOR: %^{author-or-editor}
:NOTER_DOCUMENT: %^{file}
:NOTER_PAGE:
:END:\n\n"
           :if-new (file+head "ref/${citekey}.org" ":PROPERTIES:
:ROAM_REFS: cite:${citekey}
:END:
#+TITLE: ${title}\n")
           :unnarrowed t)))
