;; -*- lexical-binding: t; -*-

(use-package org-drill
  :defer t
  :custom
  (org-drill-scope 'directory)
  (org-drill-spaced-repetition-algorithm 'simple8)
  (org-drill-add-random-noise-to-intervals-p t)
  (org-drill-adjust-intervals-for-early-and-late-repetitions-p t))

(use-package org-fc
  :disabled
  :straight (org-fc :host github :repo "l3kn/org-fc" :files (:defaults "awk" "demo.org"))
  :after org
  :custom
  (org-fc-directories (list
                       cat-org-directory
                       (substring cat-org-roam-directory 0 -1)))
  (org-fc-review-history-file (concat cat-etc-dir "org-fc-reviews.tsv")))

(use-package org-anki
  :disabled
  :after org)

(use-package promise
  :disabled
  :after org-anki)

(defvar-keymap org-srs-map
  :doc "Keymap for `org' srs packages."
  :name "Org SRS"
  :prefix 'org-srs-prefix
  "d" #'org-drill
  "a" #'org-drill-again
  "c" #'org-drill-cram)
