(defvar cat-org-roam-directory "~/org-roam/" "See `org-roam-directory'.")
(defvar cat-org-roam-dailies-directory "daily/" "See `org-roam-dailies-directory'.")
(defvar cat-org-roam-references-directory "ref/")
(defvar cat-org-roam-dailies-dir
  (concat cat-org-roam-directory cat-org-roam-dailies-directory)
  "Full path for org roam daily files.")
(defvar cat-org-roam-references-dir
  (concat cat-org-roam-directory cat-org-roam-references-directory)
  "Full path for org roam reference files.")

(use-package org-roam
  :defer t
  :init
  (setq org-roam-v2-ack t
	org-roam-directory cat-org-roam-directory
        org-roam-db-location (expand-file-name "org-roam.db" cat-etc-dir)
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
         ("C-c r c" . #'org-roam-capture))
  :bind-keymap
  ("C-c r d" . org-roam-dailies-map)
  :custom
  (org-roam-dailies-directory cat-org-roam-dailies-directory)
  :config
  (org-roam-db-autosync-mode)
  (org-roam-update-org-id-locations))

(straight-use-package '(org-roam :type built-in))

(use-package org-roam-ui
  :straight (org-roam-ui
	     :host github
	     :repo "org-roam/org-roam-ui"
	     :branch "main"
	     :files ("*.el" "out"))
  :after org-roam
  :bind ("C-c r u" . #'org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(straight-use-package '(bibtex-completion :type built-in))
(use-package org-roam-bibtex
  :straight (org-roam-bibtex :fork t)
  :after org-roam
  :config
  (setq orb-citekey-format "@%s"
	orb-file-field-extensions '("pdf" "docx" "doc" "epub"))
  (require 'ox)
  (add-to-list 'orb-preformat-keywords "title")
  (add-to-list
   'org-roam-capture-templates
   '("r" "bibliography reference" plain
     (file "org-roam-bibtex-template.org")
     :target
     (file+head "ref/${citekey}.org" "#+title: ${title}\n")
     :unnarrowed t))
  (org-roam-bibtex-mode +1))
