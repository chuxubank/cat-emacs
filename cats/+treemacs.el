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
  :after treemacs
  :config
  (treemacs-set-scope-type 'Tabs))

(use-package treemacs-nerd-icons
  :demand t
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

(defun cat-treemacs-close ()
  (interactive)
  (when (and (featurep 'treemacs)
             (treemacs-get-local-window))
    (treemacs-select-window)
    (treemacs-kill-buffer)))
