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

(use-package promise
  :after org-anki)

(use-package org-anki
  :after org)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c n a s") #'org-anki-sync-entry)
  (define-key org-mode-map (kbd "C-c n a c") #'org-anki-cloze-dwim))
