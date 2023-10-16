;; -*- lexical-binding: t; -*-

(use-package treemacs
  :defer t
  :custom
  (treemacs-persist-file (concat cat-cache-dir "treemacs")))

(use-package treemacs-magit
  :after treemacs magit)

(use-package treemacs-tab-bar
  :after treemacs tab-bar)
