;; -*- lexical-binding: t; -*-

(defvar-keymap cat-toggle-map
  :doc "Keymap for toggle commands."
  :name "Cat Toggle"
  :prefix 'cat-toggle-prefix
  "d" #'toggle-debug-on-error
  "f" #'display-fill-column-indicator-mode
  "l" #'display-line-numbers-mode
  "m" #'cat-toggle-minor-modes
  "s" #'whitespace-mode
  "t" #'toggle-truncate-lines
  "v" #'visual-line-mode
  "V" #'view-mode
  "w" #'toggle-word-wrap)

(defvar-keymap cat-plugin-map
  :doc "Keymap for plugins."
  :name "Cat Plugin"
  :prefix 'cat-plugin-prefix
  "b" #'bbdb
  "c" 'chezmoi-prefix
  "d" #'devdocs-lookup
  "D" 'docker
  "g" #'igist-dispatch
  "l" #'elogcat
  "m" #'mu4e
  "p" #'pass
  "r" #'elfeed
  "R" #'magit-list-repositories
  "t" 'telega-prefix
  "v" #'vterm
  "w" #'wl)

(defvar-keymap cat-org-plugin-map
  :doc "Keymap for `org' plugins."
  :name "Cat Org Plugin"
  :prefix 'cat-org-plugin-prefix
  "b" #'orb-note-actions
  "m" #'org-media-note-hydra/body
  "n" #'org-noter
  "j" #'org-jira-todo-to-jira
  "l" #'org-cliplink
  "s" 'org-srs-prefix)

(with-eval-after-load 'org
  (defvar-keymap cat-org-map
    :keymap org-mode-map
    "C-c n" 'cat-org-plugin-prefix))

(defvar-keymap cat-map
  :keymap mode-specific-map
  "a" #'org-agenda
  "c" #'org-capture
  "d" 'cat-org-roam-dailies-prefix
  "f" 'cat-file-prefix
  "j" 'cat-org-jira-prefix
  "l" #'org-store-link
  "o" #'ace-window
  "p" 'cat-plugin-prefix
  "r" 'cat-org-roam-prefix
  "s" #'rg-menu
  "t" 'cat-toggle-prefix
  "w" #'webjump
  "." 'cat-dict-prefix)
