(use-package org-roam
  :commands #'org-roam-buffer-toggle
  :init
  (setq org-roam-v2-ack t
	org-roam-directory (file-truename "~/org-roam")
	org-agenda-text-search-extra-files (directory-files-recursively org-roam-directory "\\.org$")
	org-roam-completion-everywhere t
	org-roam-link-auto-replace nil
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
  (org-roam-setup)
  (defun org-roam-backlinks-get (node)
    "Return the backlinks for NODE."
    (let ((backlinks (org-roam-db-query
                      [:select [source dest pos properties]
			       :from links
			       :where (or (and (= type "id")
					       (= dest $s1))
					  (and (= type "roam")
					       (= dest $s2)))]
		      (org-roam-node-title node)
		      (org-roam-node-title node))))
      (cl-loop for backlink in backlinks
	       collect (pcase-let ((`(,source-id ,dest-id ,pos ,properties) backlink))
			 (org-roam-populate
                          (org-roam-backlink-create
                           :source-node (org-roam-node-create :id source-id)
                           :target-node (org-roam-node-create :id dest-id)
                           :point pos
                           :properties properties)))))))
