;; -*- lexical-binding: t; -*-

(use-package treemacs
  :bind
  ("M-0" . treemacs-select-window)
  :custom
  (treemacs-persist-file (concat cat-cache-dir "treemacs"))
  (treemacs-is-never-other-window t))

(use-package treemacs-magit
  :after treemacs magit)

(use-package treemacs-tab-bar
  :after treemacs
  :config
  (treemacs-set-scope-type 'Tabs))
