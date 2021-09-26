(defvar cat-default-bibliography-files '("~/Zotero/My Library.bib"))

(use-package bibtex-completion
  :defer t)

(use-package bibtex-actions
  :bind (("C-c b" . bibtex-actions-insert-citation)
         :map minibuffer-local-map
         ("M-b" . bibtex-actions-insert-preset))
  :config
  (setq bibtex-actions-bibliography cat-default-bibliography-files))

(use-package org-ref
  :after org
  :init
  (setq org-ref-completion-library #'org-ref-reftex)
  :config
  (setq org-ref-default-bibliography cat-default-bibliography-files))

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
