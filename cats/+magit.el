;; -*- lexical-binding: t; -*-

(use-package magit
  :defer t
  :bind
  ("C-c p l" . #'magit-list-repositories)
  :init
  (setq transient-levels-file (concat cat-etc-dir "transient/levels")
        transient-values-file (concat cat-etc-dir "transient/values")
        transient-history-file (concat cat-etc-dir "transient/history")
	magit-repository-directories '(("~/Developer/" . 3)))
  :config
  (setq magit-diff-refine-hunk t
	magit-diff-refine-ignore-whitespace nil))

(use-package forge
  :after magit
  :config
  (setq forge-database-file (concat cat-etc-dir "forge.db")))
