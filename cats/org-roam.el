(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/org-roam"))
  :bind (("C-c n r f" . org-roam-find-file)
	 :map org-roam-mode-map
         ("C-c n r r" . org-roam)
         ("C-c n r g" . org-roam-graph)
         :map org-mode-map
         ("C-c n r i" . org-roam-insert)
         ("C-c n r I" . org-roam-insert-immediate)))
