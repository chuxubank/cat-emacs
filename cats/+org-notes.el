;; -*- lexical-binding: t; -*-

(use-package org-media-note
  :vc (org-media-note
       :url "https://github.com/yuchen-lea/org-media-note"
       :rev :newest)
  :hook (org-mode . org-media-note-mode)
  :mode-hydra
  (org-mode
   ("Note"
    (("m" org-media-note-hydra/body "media"))))
  :config
  (require 'org-attach))

(use-package org-noter
  :mode-hydra
  (org-mode
   ("Note"
    (("n" org-noter "noter"))))
  :config
  (setq org-noter--inhibit-location-change-handler t)
  (defun +org-noter-update-page-info (&optional arg)
    "Update the (precise) page info of the current note."
    (interactive "P")
    (org-entry-put nil org-noter-property-note-location
                   (org-noter--pretty-print-location
                    (org-noter--doc-approx-location
                     (when arg (org-noter--get-precise-info))))))

  (define-key org-noter-notes-mode-map (kbd "M-i") #'+org-noter-update-page-info))
