;; -*- lexical-binding: t; -*-

(defvar-keymap cat-file-map
  :doc "Keymap for file commands."
  :name "Cat File"
  :prefix 'cat-file-prefix
  "d" #'+delete-file-and-buffer
  "e" #'+find-emacs-profile
  "f" #'find-function
  "l" #'find-library
  "o" #'cat/find-org-files
  "O" #'consult-org-agenda
  "r" #'recentf-open-files)

(defvar-keymap cat-toggle-map
  :doc "Keymap for toggle commands."
  :name "Cat Toggle"
  :prefix 'cat-toggle-prefix
  "d" #'toggle-debug-on-error
  "f" #'display-fill-column-indicator-mode
  "l" #'display-line-numbers-mode
  "m" #'cat/toggle-minor-modes
  "s" #'whitespace-mode
  "t" #'toggle-truncate-lines
  "T" #'toggle-tab-bar-mode-from-frame
  "v" #'visual-line-mode
  "V" #'view-mode
  "w" #'toggle-word-wrap)

(defvar-keymap cat-plugin-map
  :doc "Keymap for plugins."
  :name "Cat Plugin"
  :prefix 'cat-plugin-prefix
  "a" #'mode-transient/minor/android-mode
  "b" #'cat-blog
  "c" 'chezmoi-transient
  "d" 'cat-dev-doc-prefix
  "D" 'docker
  "g" #'cat-git-misc
  "G" #'cat-github
  "i" #'cat-im
  "j" #'jira-issues
  "m" #'cat-mail
  "M" #'osm-prefix-map
  "n" #'deft
  "o" #'cat-oj
  "p" #'pass
  "P" #'password-store-menu
  "r" #'elfeed
  "R" #'magit-list-repositories
  "t" #'cat-term
  "w" #'webjump)

(defvar-keymap cat-map
  :keymap mode-specific-map
  ":" #'avy-goto-char-timer
  "a" #'org-agenda
  "c" #'org-capture
  "d" 'cat-org-roam-dailies-prefix
  "e" #'cat-eudc
  "f" 'cat-file-prefix
  "j" 'cat-org-jira-prefix
  "l" #'cat-language
  "L" #'org-store-link
  "n" #'mode-transient
  "o" #'ace-window
  "p" 'cat-plugin-prefix
  "P" #'cat-profiler
  "r" 'cat-org-roam-prefix
  "s" #'rg-menu
  "t" 'cat-toggle-prefix
  "v" #'cat-vibe
  "w" #'cat-workspace)

(defvar-keymap cat-escape-map
  :keymap esc-map
  "p" #'cat-cape-prefix)
