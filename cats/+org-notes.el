;; -*- lexical-binding: t; -*-

(use-package org-media-note
  :vc (org-media-note
       :url "https://github.com/yuchen-lea/org-media-note"
       :rev :newest)
  :hook (org-mode . org-media-note-mode)
  :config
  (require 'org-attach))

(use-package org-noter
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

(use-package deft
  :custom
  (deft-directory cat-org-roam-directory)
  (deft-recursive t)
  (deft-strip-summary-regexp (concat "\\("
                                     "[\n\t]" ;; blank
                                     "\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
                                     "\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n"
                                     "\\)"))
  :config
  (defun +deft-parse-org-title (file contents)
    "Parse the given FILE and CONTENTS and determine the title.
  If `deft-use-filename-as-title' is nil, the title is taken to
  be the first non-empty line of the FILE.  Else the base name of the FILE is
  used as title."
    (let ((begin (string-match "^#\\+[tT][iI][tT][lL][eE]: .*$" contents)))
      (if begin
          (string-trim (substring contents begin (match-end 0)) "#\\+[tT][iI][tT][lL][eE]: *" "[\n\t ]+")
        (deft-base-filename file))))
  (advice-add 'deft-parse-title :override #'+deft-parse-org-title))
