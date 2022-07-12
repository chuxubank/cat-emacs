;; -*- lexical-binding: t; -*-

(setq reftex-default-bibliography cat-default-bibliography-files
      org-cite-global-bibliography cat-default-bibliography-files
      org-cite-csl-styles-dir cat-default-csl-styles-dir
      org-cite-insert-processor 'citar
      org-cite-follow-processor 'citar
      org-cite-activate-processor 'citar)

(use-package citeproc
  :defer t)

(use-package citar
  :defer t
  :init
  (setq citar-bibliography cat-default-bibliography-files
	citar-notes-paths (list (concat cat-org-roam-directory cat-org-roam-reference-directory)))
  :config
  (setq citar-at-point-function 'embark-act
        citar-file-note-org-include '(org-id org-roam-ref)
	citar-file-open-function #'org-open-file)
  (set-face-font 'citar-highlight cat-mono-font))
