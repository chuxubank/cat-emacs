;; -*- lexical-binding: t; -*-

(setq reftex-default-bibliography cat-default-bibliography-files
      org-cite-global-bibliography cat-default-bibliography-files
      org-cite-csl-styles-dir cat-default-csl-styles-dir
      org-cite-insert-processor 'citar
      org-cite-follow-processor 'citar
      org-cite-activate-processor 'citar)

(use-package citar
  :defer t
  :init
  (setq citar-bibliography cat-default-bibliography-files
	citar-notes-paths (list (concat cat-org-roam-directory cat-org-roam-reference-directory)))
  :custom
  (citar-at-point-function 'embark-act))

(use-package citar-org-roam
  :after citar org-roam
  :custom
  (citar-org-roam-capture-template-key "bn")
  (citar-org-roam-subdir cat-org-roam-reference-directory)
  :config
  (citar-org-roam-mode))

(use-package citar-embark
  :after citar embark
  :config
  (citar-embark-mode))
