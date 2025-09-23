;; -*- lexical-binding: t; -*-

(defcustom cat-org-roam-directory
  (or (getenv "ORG_ROAM_DIR") "~/Developer/Personal/org-roam/")
  "Filename of the `org-roam' folder.
See `org-roam-directory'."
  :type 'directory
  :group 'cat-emacs)

(defcustom cat-org-roam-dailies-directory
  (or (getenv "ORG_ROAM_DAILIES_DIR") "daily/")
  "Path to daily-notes.
This path is relative to `org-roam-directory'.
See `org-roam-dailies-directory'."
  :type 'directory
  :group 'cat-emacs)

(defcustom cat-org-roam-default-templates-dir
  (concat cat-org-roam-directory (or (getenv "ORG_ROAM_TEMPLATES_DIR") "templates/"))
  "Path to `org-roam' templates."
  :type 'directory
  :group 'cat-emacs)

(defcustom cat-org-roam-default-roam-dir
  (concat cat-org-roam-directory (or (getenv "ORG_ROAM_ROAM_DIR") "roam/"))
  "Default roam files."
  :type 'directory
  :group 'cat-emacs)

(use-package org-roam
  :custom
  (org-roam-directory cat-org-roam-directory)
  (org-roam-completion-everywhere t)
  (org-roam-node-display-template (concat "${type:30} " (propertize "${tags:15} " 'face 'org-tag) "${title:*} "))
  (org-roam-mode-section-functions (list #'org-roam-backlinks-section
                                         #'org-roam-reflinks-section
                                         #'org-roam-unlinked-references-section))
  (org-roam-dailies-directory cat-org-roam-dailies-directory)
  (org-roam-capture-ref-templates `(("r" "Protocol Capture Reference" plain "${body}%?"
                                     :target (file+head "capture/${Input file name}.org" "#+title: ${title}\n")
                                     :unnarrowed t)
                                    ("c" "Course" plain (file ,(concat cat-org-roam-default-templates-dir "course.org"))
                                     :target (file "roam/course/${SOURCE|cmu|mit}/${COURSE-ID}.org")
                                     :unnarrowed t)
                                    ("l" "LeetCode" plain (file ,(concat cat-org-roam-default-templates-dir "leetcode.org"))
                                     :target (file "roam/cs/oj/leetcode/${number}/${title-slug}.org")
                                     :unnarrowed t)))
  :pretty-hydra
  ((:color teal :title (+with-icon "nf-md-map_marker_path" "Org Roam"))
   ("Action"
    (("r" org-roam-buffer-toggle "toggle")
     ("e" org-roam-extract-subtree "extract")
     ("R" cat-org-roam-relocate-file "relocate"))
    "Tags"
    (("at" #'org-roam-tag-add "add tag")
     ("dt" #'org-roam-tag-remove "remove tag"))
    "Alias"
    (("aa" #'org-roam-alias-add "add alias")
     ("da" #'org-roam-alias-remove "remove alias"))
    "Reference"
    (("ar" #'org-roam-ref-add "add ref")
     ("dr" #'org-roam-ref-remove "remove ref"))))
  :mode-hydra
  (org-mode
   ("Notes"
    (("r" org-roam-hydra/body "roam"))))
  :config
  (+mkdir-p cat-org-roam-directory)
  (org-roam-db-autosync-mode)
  (org-roam-update-org-id-locations)
  (require 'org-roam-protocol)
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE.

For display the directory
Ref: https://www.reddit.com/r/emacs/comments/veesun/comment/icsfzuw"
    (condition-case nil
        (directory-file-name
         (file-name-directory
          (file-relative-name (org-roam-node-file node) cat-org-roam-default-roam-dir)))
      (error ""))))

(defun cat-org-roam-get-template (&optional dir)
  "Locate a template file based on DIR.
If called interactively, open the selected template file.
If called non-interactively, return the file name of the selected template.
DIR specifies a subdirectory under `cat-org-roam-default-templates-dir'."
  (interactive)
  (let ((template-dir (concat cat-org-roam-default-templates-dir dir)))
    (if-let* ((dir-exists (file-directory-p template-dir))
              (files (directory-files-recursively template-dir ".*\\.org")))
        (let ((chosen-file (completing-read "Choose template: " files nil t)))
          (if (called-interactively-p 'any)
              (find-file chosen-file)
            chosen-file))
      (user-error "No .org templates found or directory does not exist: %s" template-dir))))

(use-package org-roam-ui
  :delight
  (org-roam-ui-mode " 󰴠")
  (org-roam-ui-follow-mode " 󰓾")
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
  "R" #'org-roam-buffer-display-dedicated
  "u" #'org-roam-ui-mode
  "t" #'cat-org-roam-get-template)

(defvar-keymap org-roam-dailies-map
  :doc "Keymap for `org-roam-dailies' commands.
Distinguish with original `org-roam-dailies-map'."
  :name "Org-roam Dailies"
  :prefix 'cat-org-roam-dailies-prefix
  "." #'org-roam-dailies-goto-today
  "d" #'org-roam-dailies-find-directory
  "b" #'org-roam-dailies-goto-previous-note
  "c" #'org-roam-dailies-capture-today
  "C" #'org-roam-dailies-capture-date
  "g" #'org-roam-dailies-goto-date
  "f" #'org-roam-dailies-goto-next-note
  ">" #'org-roam-dailies-goto-tomorrow
  "<" #'org-roam-dailies-goto-yesterday)
