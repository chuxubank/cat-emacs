;; -*- lexical-binding: t; -*-

(use-package org-drill
  :custom
  (org-drill-scope 'directory)
  (org-drill-spaced-repetition-algorithm 'simple8)
  (org-drill-add-random-noise-to-intervals-p t)
  (org-drill-adjust-intervals-for-early-and-late-repetitions-p t)
  :pretty-hydra
  ((:color teal)
   ("Drill"
    (("d" org-drill "drill")
     ("a" org-drill-again "again")
     ("c" org-drill-cram "cram"))))
  (org-mode
   ("SRS"
    (("d" org-drill-hydra/body "drill"))))
  :config
  (add-to-list 'org-tag-alist
               (cons org-drill-question-tag ?d)))

(use-package org-fc
  :disabled
  :vc (org-fc
       :url "https://github.com/l3kn/org-fc"
       :rev :newest)
  :demand t
  :after org
  :custom
  (org-fc-directories (list
                       cat-org-directory
                       (substring cat-org-roam-directory 0 -1)))
  (org-fc-review-history-file (concat cat-etc-dir "org-fc-reviews.tsv"))
  :mode-hydra
  (org-mode
   ("SRS"
    (("fd" org-fc-dashboard "fc dashboard")
     ("fh" org-fc-hydra "fc hydra")))))

(use-package org-anki
  :pretty-hydra
  ((:color teal)
   ("Anki"
    (("a" #'org-anki-sync-entry "sync entry")
     ("b" #'org-anki-browse-entry "browse entry")
     ("c" #'org-anki-cloze-dwim "cloze dwim")
     ("d" #'org-anki-delete-entry "delete entry")
     ("D" #'org-anki-delete-all "delete all")
     ("i" #'org-anki-import-deck "import deck")
     ("s" #'org-anki-sync-all "sync all")
     ("u" #'org-anki-update-all "update all"))))
  (org-mode
   ("SRS"
    (("a" org-anki-hydra/body "anki")))))

(use-package promise
  :disabled
  :after org-anki)
