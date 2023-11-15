;; -*- lexical-binding: t; -*-

(use-package transient
  :defer t
  :custom
  (transient-levels-file (concat cat-etc-dir "transient/levels"))
  (transient-values-file (concat cat-etc-dir "transient/values"))
  (transient-history-file (concat cat-etc-dir "transient/history")))

(use-package magit
  :defer t
  :custom
  (magit-blame-mode-lighter " ")
  (magit-repository-directories '(("~/Developer/" . 5)))
  (magit-diff-refine-hunk t)
  (magit-diff-refine-ignore-whitespace nil))

(use-package forge
  :after magit
  :custom
  (forge-database-file (concat cat-etc-dir "forge.db")))
