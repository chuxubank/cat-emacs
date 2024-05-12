;; -*- lexical-binding: t; -*-

(use-package oc
  :ensure nil
  :custom
  (org-cite-global-bibliography cat-default-bibliography-files)
  (org-cite-csl-styles-dir cat-default-csl-styles-dir)
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar))

(use-package bibtex-completion
  :custom
  (bibtex-completion-additional-search-fields '(keywords))
  (bibtex-completion-pdf-field "file")
  (bibtex-completion-bibliography cat-default-bibliography-files)
  :config
  (bibtex-completion-init))

(defun +orb-note-update-file (citekey)
  "Update the `org-noter-property-doc-file' property of the CITEKEY."
  (citar--library-file-action
   citekey
   (lambda (file)
     (org-entry-put nil org-noter-property-doc-file (abbreviate-file-name file)))))

(defun +orb-note-citar (citekey)
  "Use `citar' to open the CITEKEY note file."
  (citar-run-default-action (ensure-list citekey)))

(defun cat-org-roam-allowed-directory-p (dir)
  "Check whether a DIR should be listed as a filterable directory.
Hides template, daily directories."
  (and (not (string-match-p cat-org-roam-template-directory (file-name-as-directory dir)))
       (not (string-match-p cat-org-roam-dailies-directory (file-name-as-directory dir)))))

(defun cat-org-roam-locate-file (name)
  "Choose directory and return with full file NAME."
  (let* ((default-directory cat-org-roam-directory)
         (dir-list (split-string (shell-command-to-string "fd --type d .") "\n" t))
         (filter-dir-list (seq-filter #'cat-org-roam-allowed-directory-p dir-list))
         (dir (completing-read "Choose org-roam sub directory: " filter-dir-list nil 'confirm))
         (full-dir (expand-file-name dir cat-org-roam-directory))
         (filename (file-name-with-extension name "org")))
    (unless (file-directory-p full-dir)
      (make-directory full-dir t))
    (expand-file-name filename full-dir)))

(defun cat-org-roam-get-template (&optional dir)
  "Choose template based on DIR."
  (let* ((template (concat cat-org-roam-directory
                           cat-org-roam-template-directory
                           dir))
         (file (if (file-directory-p template)
                   (completing-read "Choose template: " (directory-files-recursively template ".*\\.org") nil t)
                 template)))
    (org-file-contents file)))

(defun cat-org-roam-relocate-file ()
  "Relocate and rename the Org-roam file."
  (interactive)
  (let* ((default-directory cat-org-roam-directory)
         (node (or (org-roam-node-at-point)
                   (let ((node (org-roam-node-read)))
                     (org-roam-node-open node)
                     node)))
         (slug (org-roam-node-slug node))
         (file (org-roam-node-file node))
         (git-time (string-trim (shell-command-to-string (format "git log --diff-filter=A --follow --format='%%aI' -- %s | tail -1" file))))
         (file-time (file-attribute-modification-time (file-attributes file)))
         (time (if (string-empty-p git-time)
                   file-time
                 (let ((git-time (encode-time (parse-time-string git-time)))
                       (file-time (encode-time (decode-time file-time))))
                   (if (time-less-p git-time file-time)
                       git-time
                     file-time))))
         (new-name (concat (format-time-string "%Y%m%d%H%M%S" time)
                           "-"
                           slug))
         (new-file (cat-org-roam-locate-file new-name)))
    (rename-file file new-file)
    (kill-buffer (get-file-buffer file))
    (org-roam-db-sync)
    (org-open-file new-file)))

(use-package org-roam-bibtex
  :demand t
  :after org-roam
  :delight " 󱉟"
  :custom
  (orb-roam-ref-format 'org-cite)
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "%(cat-org-roam-locate-file \"%<%Y%m%d%H%M%S>-${slug}\")" "#+title: ${title}")
      :unnarrowed t)
     ("r" "bibliography reference")
     ("rd" "Bibliography reference default" plain "%?"
      :target (file+head "%(cat-org-roam-locate-file \"%<%Y%m%d%H%M%S>-${title}\")" "#+title: ${title}")
      :unnarrowed t)
     ("rn" "Bibliography reference with org-noter" plain (function (lambda () (cat-org-roam-get-template "org-noter.org")))
      :target (file "%(cat-org-roam-locate-file \"%<%Y%m%d%H%M%S>-${title}\")")
      :unnarrowed t)
     ("rl" "Bibliography reference with link" plain "eww:%^{url}"
      :target (file+head "%(cat-org-roam-locate-file \"%<%Y%m%d%H%M%S>-${title}\")" "#+title: ${title}\n#+date: ${date}"))
     ("rv" "Bibliography reference with video" plain "[[video:%^{url}#]]"
      :target (file+head "%(cat-org-roam-locate-file \"%<%Y%m%d%H%M%S>-${title}\")" "#+title: ${title}\n"))
     ("re" "Examination" plain (function (lambda () (cat-org-roam-get-template "exam")))
      :target (file "%(cat-org-roam-locate-file \"%<%Y%m%d%H%M%S>-${title}\")")
      :unnarrowed t)))
  :config
  (+add-to-list-multi 'orb-attached-file-extensions "docx" "doc" "epub")
  (+add-to-list-multi 'orb-preformat-keywords "title" "url")
  (+add-to-list-multi 'orb-note-actions-user
                      '("Update org-noter file" . +orb-note-update-file)
                      '("Open with citar" . +orb-note-citar))
  (org-roam-bibtex-mode 1))

(use-package citar-org-roam
  :delight
  :demand t
  :after citar org-roam-bibtex
  :custom
  (citar-org-roam-capture-template-key "rn")
  :config
  ;; https://github.com/emacs-citar/citar/wiki/Notes-configuration#org-roam-bibtex
  ;; See `citar-org-roam-notes-config'
  (citar-register-notes-source
   'orb-citar-source (list :name "Org-Roam BibTex Notes"
                           :category 'org-roam-node
                           :items #'citar-org-roam--get-candidates
                           :hasitems #'citar-org-roam-has-notes
                           :open #'citar-org-roam-open-note
                           :create #'orb-citar-edit-note
                           :annotate #'citar-org-roam--annotate))
  (setq citar-notes-source 'orb-citar-source))

(use-package citar
  :custom
  (citar-bibliography cat-default-bibliography-files))

(use-package citar-embark
  :delight
  :demand t
  :after citar embark
  :config
  (citar-embark-mode))
