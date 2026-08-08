;; -*- lexical-binding: t; -*-

(use-package org-drill
  :custom
  (org-drill-scope 'directory)
  (org-drill-spaced-repetition-algorithm 'simple8)
  (org-drill-add-random-noise-to-intervals-p t)
  (org-drill-adjust-intervals-for-early-and-late-repetitions-p t)
  :transient
  (cat-org-drill
   ["Drill"
    ("d" "drill" org-drill)
    ("a" "again" org-drill-again)
    ("c" "cram" org-drill-cram)])
  :major-transient
  (org-mode
   ["SRS"
    ("d" "drill" cat-org-drill)])
  :config
  (add-to-list 'org-tag-alist
               (cons org-drill-question-tag ?d)))

(use-package org-fc
  :cat fc
  :vc (:url "https://github.com/l3kn/org-fc")
  :demand t
  :after org
  :custom
  (org-fc-directories (list
                       cat-org-directory
                       (substring cat-org-roam-directory 0 -1)))
  (org-fc-review-history-file (concat cat-etc-dir "org-fc-reviews.tsv"))
  :major-transient
  (org-mode
   ["SRS"
    ("fd" "fc dashboard" org-fc-dashboard)
    ("fh" "fc hydra" org-fc-hydra)]))

(use-package org-anki
  :transient
  (cat-org-anki
   ["Anki"
    ("a" "sync entry" org-anki-sync-entry)
    ("b" "browse entry" org-anki-browse-entry)
    ("c" "cloze dwim" org-anki-cloze-dwim)
    ("d" "delete entry" org-anki-delete-entry)
    ("D" "delete all" org-anki-delete-all)
    ("i" "import deck" org-anki-import-deck)
    ("s" "sync all" org-anki-sync-all)
    ("u" "update all" org-anki-update-all)])
  :major-transient
  (org-mode
   ["SRS"
    ("sa" "anki" cat-org-anki)]))

(use-package promise
  :cat
  :after org-anki)
