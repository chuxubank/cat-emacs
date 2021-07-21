(use-package org-roam
  :commands #'org-roam-buffer-toggle
  :init
  (setq org-roam-v2-ack t
	org-roam-directory (file-truename "~/org-roam")
	org-agenda-text-search-extra-files (directory-files-recursively org-roam-directory "\\.org$")
	org-roam-completion-everywhere t
	org-roam-mode-section-functions
	(list #'org-roam-backlinks-section
              #'org-roam-reflinks-section
              #'org-roam-unlinked-references-section))
  :bind (("C-c r f" . org-roam-node-find)
         ("C-c r g" . org-roam-graph)
         ("C-c r i" . org-roam-node-insert)
         ("C-c r c" . org-roam-capture)
         ;; Dailies
         ("C-c r t" . org-roam-dailies-capture-today))
  :config
  (org-roam-setup))
