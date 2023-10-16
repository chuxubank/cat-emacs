;; -*- lexical-binding: t; -*-

(use-package treemacs
  :defer t
  :custom
  (treemacs-persist-file (concat cat-cache-dir "treemacs")))

(use-package treemacs-magit
  :after treemacs magit)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-tab-bar
  :after treemacs tab-bar)

(use-package treemacs-all-the-icons
  :when (or (daemonp)
            (display-graphic-p))
  :after treemacs)
