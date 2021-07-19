(setq org-roam-v2-ack t
      org-roam-directory (file-truename "~/org-roam")
      org-roam-link-auto-replace nil
      org-agenda-text-search-extra-files (directory-files-recursively org-roam-directory "\\.org$"))

(use-package org-roam
  :bind (("C-c r f" . org-roam-node-find)
         ("C-c r g" . org-roam-graph)
         ("C-c r c" . org-roam-capture)
         :map org-mode-map
         ("C-c r r" . org-roam-buffer-toggle)
         ("C-c r i" . org-roam-node-insert)))
