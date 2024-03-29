;; -*- lexical-binding: t; -*-

(use-package org-agenda
  :ensure nil
  :custom
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-include-diary t))

(use-package calendar
  :ensure nil
  :custom
  (diary-file (expand-file-name "diary" cat-org-directory)))

(use-package org-agenda-count
  :vc (org-agenda-count
       :url "https://github.com/sid-kurias/org-agenda-count"
       :rev :newest)
  :demand t
  :after org
  :custom
  (org-agenda-custom-commands
   '(("n" "Agenda and all TODOs"
      ((agenda
        ""
        ((org-agenda-span 'day)
         (org-agenda-include-diary nil)
         (org-agenda-overriding-header
          (format "Agenda [%s]" (org-agenda-count)))))
       (alltodo
        ""
        ((org-agenda-overriding-header
          (format "All TODOs [%s]" (org-agenda-count "alltodo"))))))))))

(use-package org-edna
  :delight " "
  :hook (org-mode . org-edna-mode)
  :custom
  (org-edna-finder-use-cache t))
