;; -*- lexical-binding: t; -*-

(use-package org-roam
  :commands #'org-roam-buffer-toggle
  :custom
  (org-roam-directory cat-org-roam-directory)
  (org-roam-db-location (expand-file-name "org-roam.db" cat-etc-dir))
  (org-roam-completion-everywhere t)
  (org-roam-node-display-template (concat "${type:10} " (propertize "${tags:15} " 'face 'org-tag) "${title:*} "))
  (org-roam-mode-section-functions (list #'org-roam-backlinks-section
                                         #'org-roam-reflinks-section
                                         #'org-roam-unlinked-references-section))
  (org-roam-dailies-directory cat-org-roam-dailies-directory)
  (org-roam-capture-ref-templates '(("r" "Protocol Capture Reference"
                                     plain "${body}%?" :target
                                     (file+head "capture/${Input file name}.org" "#+title: ${title}\n")
                                     :unnarrowed t)
                                    ("c" "Course"
                                     plain (file "templates/course.org") :target
                                     (file "course/${SOURCE|cmu|mit}/${COURSE-ID}.org")
                                     :unnarrowed t)))
  :config
  (+mkdir-p cat-org-roam-directory)
  (org-roam-db-autosync-mode)
  (org-roam-update-org-id-locations)
  (require 'org-roam-protocol)
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error ""))))

(use-package org-roam-ui
  :delight " 󰴠"
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t)
  (org-roam-ui-ref-title-template "%^{author-abbrev} (%^{date}) %^{title}"))

(defvar-keymap org-roam-map
  :doc "Keymap for `org-roam' commands."
  :name "Org-roam"
  :prefix 'cat-org-roam-prefix
  "c" #'org-roam-capture
  "f" #'org-roam-node-find
  "F" #'org-roam-ref-find
  "g" #'org-roam-graph
  "i" #'org-roam-node-insert
  "r" #'org-roam-buffer-toggle
  "R" #'org-roam-buffer-display-dedicated
  "u" #'org-roam-ui-mode)

(defvar-keymap org-roam-dailies-map
  :doc "Keymap for `org-roam-dailies' commands.
Distinguish with original `org-roam-dailies-map'."
  :name "Org-roam Dailies"
  :prefix 'cat-org-roam-dailies-prefix
  "." #'org-roam-dailies-goto-today
  "/" #'org-roam-dailies-find-directory
  "b" #'org-roam-dailies-goto-previous-note
  "c" #'org-roam-dailies-capture-today
  "C" #'org-roam-dailies-capture-date
  "d" #'org-roam-dailies-goto-date
  "f" #'org-roam-dailies-goto-next-note
  "t" #'org-roam-dailies-goto-tomorrow
  "y" #'org-roam-dailies-goto-yesterday)
