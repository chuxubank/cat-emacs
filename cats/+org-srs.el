;; -*- lexical-binding: t; -*-

(use-package org-drill
  :custom
  (org-drill-scope 'directory)
  (org-drill-spaced-repetition-algorithm 'simple8)
  (org-drill-add-random-noise-to-intervals-p t)
  (org-drill-adjust-intervals-for-early-and-late-repetitions-p t))

(use-package org-fc
  :disabled
  :vc (:url "https://github.com/l3kn/org-fc" :rev :newest)
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
