(use-package org-roam
  :defer t
  :init
  (setq org-roam-v2-ack t
	org-roam-directory (file-truename "~/org-roam")
	org-roam-db-location (expand-file-name "org-roam.db" cat-etc-dir)
	org-agenda-text-search-extra-files (directory-files-recursively org-roam-directory "\\.org$")
	org-roam-completion-everywhere t
	org-roam-mode-section-functions
	(list #'org-roam-backlinks-section
              #'org-roam-reflinks-section
              #'org-roam-unlinked-references-section))
  :bind (("C-c r r" . #'org-roam-buffer-toggle)
	 ("C-c r R" . #'org-roam-buffer-display-dedicated)
	 ("C-c r f" . #'org-roam-node-find)
	 ("C-c r F" . #'org-roam-ref-find)
         ("C-c r g" . #'org-roam-graph)
         ("C-c r i" . #'org-roam-node-insert)
         ("C-c r c" . #'org-roam-capture)
	 ("C-c r d" . 'org-roam-dailies-map))
  :config
  (org-roam-setup))

(straight-use-package '(org-roam :type built-in))

(use-package org-roam-ui
  :straight (org-roam-ui
	     :host github
	     :repo "org-roam/org-roam-ui"
	     :branch "main"
	     :files ("*.el" "out"))
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))
