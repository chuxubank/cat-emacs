;; -*- lexical-binding: t; -*-

(use-package org-roam
  :defer t
  :init
  (setq org-roam-v2-ack t
	org-roam-directory cat-org-roam-directory
        org-roam-db-location (expand-file-name "org-roam.db" cat-etc-dir)
	org-roam-completion-everywhere t
	org-roam-node-display-template (concat (propertize "${tags:10} " 'face 'org-tag) "${title:*} ")
	org-roam-mode-section-functions
	(list #'org-roam-backlinks-section
              #'org-roam-reflinks-section
              #'org-roam-unlinked-references-section))
  :custom
  (org-roam-dailies-directory cat-org-roam-dailies-directory)
  :config
  (org-roam-db-autosync-mode)
  (org-roam-update-org-id-locations)
  (require 'org-roam-protocol)
  (setq org-roam-capture-ref-templates
	'(("r" "Protocol Capture Reference"
	   plain "${body}%?" :target
	   (file+head "capture/${Input file name}.org" "#+title: ${title}\n")
	   :unnarrowed t)
	  ("c" "Course"
	   plain (file "templates/course.org") :target
	   (file "course/${SOURCE|cmu|mit}/${COURSE-ID}.org")
	   :unnarrowed t))))

(use-package org-roam-ui
  :defer t
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t
	org-roam-ui-ref-title-template
	"%^{author-abbrev} (%^{date}) %^{title}"))

(defvar org-roam-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" #'org-roam-capture)
    (define-key map "f" #'org-roam-node-find)
    (define-key map "F" #'org-roam-ref-find)
    (define-key map "g" #'org-roam-graph)
    (define-key map "i" #'org-roam-node-insert)
    (define-key map "R" #'org-roam-buffer-display-dedicated)
    (define-key map "u" #'org-roam-ui-mode)
    map)
  "Keymap for `org-roam' commands.")
(defalias 'org-roam-prefix org-roam-map)

(with-eval-after-load 'org-roam
  (define-key org-roam-map "r" #'org-roam-buffer-toggle))

(defvar org-roam-dailies-prefix
  (let ((map (make-sparse-keymap)))
    (define-key map "." #'org-roam-dailies-goto-today)
    (define-key map "/" #'org-roam-dailies-find-directory)
    (define-key map "b" #'org-roam-dailies-goto-previous-note)
    (define-key map "c" #'org-roam-dailies-capture-today)
    (define-key map "C" #'org-roam-dailies-capture-date)
    (define-key map "d" #'org-roam-dailies-goto-date)
    (define-key map "f" #'org-roam-dailies-goto-next-note)
    (define-key map "t" #'org-roam-dailies-goto-tomorrow)
    (define-key map "y" #'org-roam-dailies-goto-yesterday)
    map)
  "Keymap for `org-roam-dailies' commands.

Distinguish with original `org-roam-dailies-map'.")
(defalias 'org-roam-dailies-prefix org-roam-dailies-prefix)
