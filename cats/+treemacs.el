;; -*- lexical-binding: t; -*-

(use-package treemacs
  :bind
  ("M-0" . treemacs-select-window)
  :custom
  (treemacs-is-never-other-window t))

(use-package treemacs-magit
  :demand t
  :after treemacs magit)

(use-package treemacs-tab-bar
  :demand t
  :after treemacs)

(use-package treemacs-nerd-icons
  :demand t
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package treemacs-activities
  :ensure nil
  :demand t
  :after (treemacs activities)
  :config
  (treemacs-set-scope-type 'Activities))
