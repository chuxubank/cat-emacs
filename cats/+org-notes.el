;; -*- lexical-binding: t; -*-

(use-package org-media-note
  :vc (org-media-note
       :url "https://github.com/yuchen-lea/org-media-note"
       :rev :newest)
  :hook (org-mode . org-media-note-mode)
  :config
  (require 'org-attach))

(use-package mpv)

(use-package org-noter
  :custom
  ;; (org-noter-notes-window-location
  ;;  (if (string= (getenv "HOST_TYPE") "work")
  ;;      'other-frame
  ;;    'horizontal-split))
  ;; (org-noter-use-indirect-buffer nil)
  (org-noter-doc-split-fraction '(0.6 . 0.5))
  :config
  (setq org-noter--inhibit-location-change-handler t)
  (defun +org-noter-update-page-info (&optional arg)
    "Update the (precise) page info of the current note."
    (interactive "P")
    (org-entry-put nil org-noter-property-note-location
                   (org-noter--pretty-print-location
                    (org-noter--doc-approx-location
                     (when arg (org-noter--get-precise-info))))))
  (define-key org-noter-notes-mode-map (kbd "M-i") #'+org-noter-update-page-info)

  (with-eval-after-load 'org-roam
    (org-noter-enable-org-roam-integration))

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
  (add-to-list 'org-noter-parse-document-property-hook #'org-noter-orb-citar-find-document-from-refs))

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
