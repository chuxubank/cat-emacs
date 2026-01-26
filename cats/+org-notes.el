;; -*- lexical-binding: t; -*-

(use-package org-media-note
  :vc (:url "https://github.com/yuchen-lea/org-media-note")
  :hook (org-mode . org-media-note-mode)
  :mode-hydra
  (org-mode
   ("Notes"
    (("v" org-media-note-show-interface "media"))))
  :config
  (require 'org-attach))

(use-package mpv)

(use-package org-noter
  :custom
  (org-noter-notes-window-location
   (if (string= system-name "Yui.local")
       'vertical-split
     'horizontal-split))
  (org-noter-doc-split-fraction '(0.6 . 0.5))
  :mode-hydra
  (org-mode
   ("Notes"
    (("n" org-noter "noter"))))
  :config
  (setq org-noter--inhibit-location-change-handler t)
  (define-key org-noter-notes-mode-map (kbd "M-i") #'+org-noter-update-page-info)
  (add-to-list 'org-noter-parse-document-property-hook #'org-noter-orb-citar-find-document-from-refs)
  (add-to-list 'org-noter-find-additional-notes-functions #'org-noter-citar-find-key-from-this-file))

(defun +org-noter-update-page-info (&optional arg)
  "Update the (precise) page info of the current note."
  (interactive "P")
  (org-entry-put nil org-noter-property-note-location
                 (org-noter--pretty-print-location
                  (org-noter--doc-approx-location
                   (when arg (org-noter--get-precise-info))))))

(defun org-noter-orb-citar-find-document-from-refs (&optional cite-key)
  "Return a note file associated with CITE-KEY.
When there is more than one note files associated with CITE-KEY, have
user select one of them."
  (let* ((key (or cite-key (orb-get-node-citekey)))
         (file-table (citar-get-files key))
         (files '()))
    (when file-table
      (maphash (lambda (key file-list)
                 (setq files (append files file-list)))
               file-table))
    (cond ((= (length files) 1)
           (car files))
          ((> (length files) 1)
           (completing-read (format "Which document from %s?: " key) files)))))

(defun org-noter-citar-find-key-from-this-file (filename)
  (let (entry-alist)
    (maphash (lambda (key entry)
               (when-let ((file (citar-get-value citar-file-variable entry)))
                 (push (cons file key) entry-alist)))
             (citar-get-entries))
    (let ((key (alist-get filename entry-alist nil nil (lambda (s regexp)
                                                         (string-match-p regexp s)))))
      (orb-find-note-file key))))

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
