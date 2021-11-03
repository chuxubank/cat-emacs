(use-package org-drill
  :disabled
  :defer t
  :custom
  (org-drill-scope 'agenda))

(use-package org-fc
  :straight (org-fc :host github :repo "l3kn/org-fc" :files (:defaults "awk" "demo.org"))
  :after org
  :custom
  (org-fc-directories (list
		       cat-org-directory
		       cat-org-roam-directory
		       cat-org-roam-dailies-dir
		       cat-org-roam-references-dir)))
