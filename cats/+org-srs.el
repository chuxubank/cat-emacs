;; -*- lexical-binding: t; -*-

(use-package org-drill
  :custom
  (org-drill-scope 'directory)
  (org-drill-spaced-repetition-algorithm 'simple8)
  (org-drill-add-random-noise-to-intervals-p t)
  (org-drill-adjust-intervals-for-early-and-late-repetitions-p t)
  :mode-hydra
  (org-mode
   ("Drill"
    (("dd" org-drill "drill")
     ("da" org-drill-again "again")
     ("dc" org-drill-cram "cram")))))

(use-package org-fc
  :disabled
  :vc (org-fc
       :url "https://github.com/l3kn/org-fc"
       :rev :newest)
  :after org
  :custom
  (org-fc-directories (list
                       cat-org-directory
                       (substring cat-org-roam-directory 0 -1)))
  (org-fc-review-history-file (concat cat-etc-dir "org-fc-reviews.tsv")))

(use-package org-anki
  :mode-hydra
  (org-mode
   ("Anki"
    (("aa" #'org-anki-sync-entry "sync entry")
     ("ab" #'org-anki-browse-entry "browse entry")
     ("ac" #'org-anki-cloze-dwim "cloze dwim")
     ("ad" #'org-anki-delete-entry "delete entry")
     ("aD" #'org-anki-delete-all "delete all")
     ("ai" #'org-anki-import-deck "import deck")
     ("as" #'org-anki-sync-all "sync all")
     ("au" #'org-anki-update-all "update all")))))

(use-package promise
  :disabled
  :after org-anki)
