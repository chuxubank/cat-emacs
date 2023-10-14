;; -*- lexical-binding: t; -*-
(setq reftex-default-bibliography cat-default-bibliography-files
      org-cite-global-bibliography cat-default-bibliography-files
      org-cite-csl-styles-dir cat-default-csl-styles-dir
      org-cite-insert-processor 'citar
      org-cite-follow-processor 'citar
      org-cite-activate-processor 'citar
      bibtex-completion-additional-search-fields '(keywords)
      bibtex-completion-pdf-field "file"
      bibtex-completion-bibliography cat-default-bibliography-files)

(defun cat-orb-note-file ()
  "Return the path to the `org-roam-bibtex' note file"
  (concat cat-org-roam-reference-directory "${citekey}.org"))

(defun +orb-note-update-file (citekey)
  "Update the `org-noter-property-doc-file' property of the CITEKEY"
  (citar--library-file-action
   citekey
   (lambda (file)
     (org-entry-put nil org-noter-property-doc-file (abbreviate-file-name file)))))

(defun +orb-note-citar (citekey)
  "Use `citar' to open the CITEKEY note file"
  (citar-run-default-action (ensure-list citekey)))

(use-package org-roam-bibtex
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :custom
  (orb-roam-ref-format 'org-cite)
  :config
  (+add-to-list-multi 'orb-attached-file-extensions "docx" "doc" "epub")
  (+add-to-list-multi 'orb-preformat-keywords "title" "url")
  (+add-to-list-multi 'orb-note-actions-user
                      '("Update org-noter file" . +orb-note-update-file)
                      '("Open with citar" . +orb-note-citar))
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
           :unnarrowed t)
          ("r" "bibliography reference")
          ("rd" "Bibliography reference default" plain "%?"
           :target (file+head "%(cat-orb-note-file)" "#+title: ${title}")
           :unnarrowed t)
          ("rn" "Bibliography reference with org-noter" plain (file "templates/org-noter.org")
           :target (file "%(cat-orb-note-file)")
           :unnarrowed t)
          ("rl" "Bibliography reference with link" plain "eww:%^{url}"
           :target (file+head "%(cat-orb-note-file)" "#+title: ${title}\n#+date: ${date}"))
          ("rv" "Bibliography reference with video" plain "[[video:%^{url}#]]"
           :target (file+head "%(cat-orb-note-file)" "#+title: ${title}\n"))
          ("rx" "SCSEE XingCe" plain (file "templates/xingce.org")
           :target (file "%(cat-orb-note-file)")
           :unnarrowed t)
          ("rs" "SCSEE ShenLun" plain (file "templates/shenlun.org")
           :target (file "%(cat-orb-note-file)")
           :unnarrowed t))))

(use-package citar-org-roam
  :hook (org-roam-mode . citar-org-roam-mode)
  :custom
  (citar-org-roam-capture-template-key "bn")
  (citar-org-roam-subdir cat-org-roam-reference-directory)
  (citar-notes-paths (list (concat cat-org-roam-directory cat-org-roam-reference-directory)))
  :config
  (citar-register-notes-source
   'orb-citar-source (list :name "Org-Roam BibTex Notes"
                           :category 'org-roam-node
                           :items #'citar-org-roam--get-candidates
                           :hasitems #'citar-org-roam-has-notes
                           :open #'citar-org-roam-open-note
                           :create #'orb-citar-edit-note
                           :annotate #'citar-org-roam--annotate))
  (setq citar-notes-source 'orb-citar-source)
  (+change-lighter 'citar-org-roam-mode nil))

(use-package citar
  :defer t
  :custom
  (citar-bibliography cat-default-bibliography-files))

(use-package citar-embark
  :after citar embark
  :config
  (citar-embark-mode)
  (+change-lighter 'citar-embark-mode nil))
