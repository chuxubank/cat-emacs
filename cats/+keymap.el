;; -*- lexical-binding: t; -*-

(defvar-keymap cat-file-map
  :doc "Keymap for file commands."
  :name "Cat File"
  :prefix 'cat-file-prefix
  "d" #'+delete-file-and-buffer
  "e" #'+find-emacs-profile
  "f" #'find-function
  "l" #'find-library
  "o" #'+find-org-files
  "O" #'consult-org-agenda
  "r" #'recentf-open-files)

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
  "T" #'toggle-tab-bar-mode-from-frame
  "v" #'visual-line-mode
  "V" #'view-mode
  "w" #'toggle-word-wrap)

(defvar-keymap cat-plugin-map
  :doc "Keymap for plugins."
  :name "Cat Plugin"
  :prefix 'cat-plugin-prefix
  "b" #'cat-blog/body
  "c" 'chezmoi-prefix
  "d" 'cat-dev-doc-prefix
  "D" 'docker
  "g" #'cat-git-misc/body
  "G" #'cat-github/body
  "i" #'cat-im/body
  "j" #'jira-issues
  "l" #'elogcat
  "m" #'cat-mail/body
  "M" #'osm-prefix-map
  "n" #'deft
  "o" #'cat-oj/body
  "p" #'pass
  "P" #'password-store-menu
  "r" #'elfeed
  "R" #'magit-list-repositories
  "w" #'webjump)

(defvar-keymap cat-map
  :keymap mode-specific-map
  ":" #'avy-goto-char-timer
  "a" #'org-agenda
  "A" #'cat-agent/body
  "c" #'org-capture
  "d" 'cat-org-roam-dailies-prefix
  "e" #'cat-eudc/body
  "f" 'cat-file-prefix
  "j" 'cat-org-jira-prefix
  "l" #'cat-language/body
  "L" #'org-store-link
  "n" #'major-mode-hydra
  "o" #'ace-window
  "p" 'cat-plugin-prefix
  "P" #'profiler-hydra/body
  "r" 'cat-org-roam-prefix
  "s" #'rg-menu
  "t" 'cat-toggle-prefix
  "T" #'cat-term/body
  "v" #'cat-vibe/body
  "w" #'cat-workspace/body)

(defvar-keymap cat-escape-map
  :keymap esc-map
  "p" #'cat-cape-prefix)
