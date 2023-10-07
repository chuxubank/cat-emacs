;; -*- lexical-binding: t; -*-

(use-package bbdb
  :defer t
  :custom
  (bbdb-file (expand-file-name "bbdb" cat-org-directory))
  :config
  (bbdb-initialize))
