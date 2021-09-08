(use-package magit
  :defer t
  :init
  (setq transient-levels-file (concat cat-etc-dir "transient/levels")
        transient-values-file (concat cat-etc-dir "transient/values")
        transient-history-file (concat cat-etc-dir "transient/history"))
  :config
  (setq magit-diff-refine-hunk t
	magit-diff-refine-ignore-whitespace nil))
