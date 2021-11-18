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
(straight-use-package '(bibtex-completion :type built-in))

(use-package citeproc
  :defer t)

(use-package citar-org
  :ensure citar
  :after org
  :config
  (setq citar-at-point-function 'embark-act
	citar-bibliography cat-default-bibliography-files
	citar-file-note-org-include '(org-id org-roam-ref)
	citar-notes-paths (list cat-org-roam-references-dir)
	citar-file-open-function #'org-open-file
	org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar)
  (citar-filenotify-setup '(LaTeX-mode-hook org-mode-hook)))
