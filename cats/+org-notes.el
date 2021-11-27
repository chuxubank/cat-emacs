;; -*- lexical-binding: t; -*-

(use-package org-media-note
  :straight (org-media-note :host github :repo "yuchen-lea/org-media-note")
  :hook (org-mode . org-media-note-mode)
  :config
  (require 'org-attach))

(use-package org-noter
  :defer t
  :config
  (defun org-noter-update-precise-page-info ()
    (interactive)
    (org-entry-put nil
     org-noter-property-note-location
     (org-noter--pretty-print-location
      (org-noter--doc-approx-location (org-noter--get-precise-info)))))
  (define-key org-noter-notes-mode-map (kbd "M-i") #'org-noter-update-precise-page-info))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c n m") #'org-media-note-hydra/body)
  (define-key org-mode-map (kbd "C-c n n") #'org-noter))
