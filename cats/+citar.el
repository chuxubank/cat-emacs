;; -*- lexical-binding: t; -*-

(setq reftex-default-bibliography cat-default-bibliography-files
      org-cite-global-bibliography cat-default-bibliography-files
      org-cite-csl-styles-dir cat-default-csl-styles-dir
      org-cite-insert-processor 'citar
      org-cite-follow-processor 'citar
      org-cite-activate-processor 'citar
      citar-bibliography cat-default-bibliography-files
      citar-notes-paths (list (concat cat-org-roam-directory cat-org-roam-reference-directory)))

(use-package citeproc
  :defer t)

(use-package citar-org
  :ensure citar
  :after org
  :config
  (setq citar-at-point-function 'embark-act
        citar-file-note-org-include '(org-id org-roam-ref)
	citar-file-open-function #'org-open-file)
  (citar-filenotify-setup '(LaTeX-mode-hook org-mode-hook)))
