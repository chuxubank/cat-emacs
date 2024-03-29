;; -*- lexical-binding: t; -*-

(use-package treemacs
  :bind
  ("M-0" . treemacs-select-window)
  :custom
  (treemacs-persist-file (concat cat-cache-dir "treemacs"))
  (treemacs-is-never-other-window t))

(use-package treemacs-magit
  :demand t
  :after treemacs magit)

(use-package treemacs-tab-bar
  :demand t
  :after treemacs
  :config
  (treemacs-set-scope-type 'Tabs))

(use-package treemacs-nerd-icons
  :demand t
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))
