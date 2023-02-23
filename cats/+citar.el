;; -*- lexical-binding: t; -*-

(setq reftex-default-bibliography cat-default-bibliography-files
      org-cite-global-bibliography cat-default-bibliography-files
      org-cite-csl-styles-dir cat-default-csl-styles-dir
      org-cite-insert-processor 'citar
      org-cite-follow-processor 'citar
      org-cite-activate-processor 'citar)

(use-package citar
  :defer t
  :custom
  (citar-bibliography cat-default-bibliography-files))

(use-package citar-embark
  :after citar embark
  :config
  (citar-embark-mode))
