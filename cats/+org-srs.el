(use-package org-drill
  :defer t
  :custom
  (org-drill-scope 'directory))

(use-package org-fc
  :disabled
  :straight (org-fc :host github :repo "l3kn/org-fc" :files (:defaults "awk" "demo.org"))
  :after org
  :custom
  (org-fc-directories (list
		       cat-org-directory
		       (substring cat-org-roam-directory 0 -1)))
  (org-fc-review-history-file (concat cat-etc-dir "org-fc-reviews.tsv")))
