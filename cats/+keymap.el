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
  "T" #'toggle-tab-bar-mode-from-frame
  "v" #'visual-line-mode
  "V" #'view-mode
  "w" #'toggle-word-wrap)

(defvar-keymap cat-plugin-map
  :doc "Keymap for plugins."
  :name "Cat Plugin"
  :prefix 'cat-plugin-prefix
  "b" #'bbdb
  "c" 'chezmoi-prefix
  "d" 'cat-dev-doc-prefix
  "D" 'docker
  "e" 'ement-prefix
  "g" #'igist-dispatch
  "l" #'elogcat
  "m" #'mu4e
  "n" #'deft
  "p" #'pass
  "r" #'elfeed
  "R" #'magit-list-repositories
  "t" 'telega-prefix
  "v" #'vterm-toggle
  "w" #'webjump)

(defvar-keymap cat-map
  :keymap mode-specific-map
  "." 'cat-dict-prefix
  ":" #'avy-goto-char-timer
  "a" #'org-agenda
  "c" #'org-capture
  "d" 'cat-org-roam-dailies-prefix
  "f" 'cat-file-prefix
  "j" 'cat-org-jira-prefix
  "l" #'org-store-link
  "n" #'major-mode-hydra
  "o" #'ace-window
  "p" 'cat-plugin-prefix
  "P" #'profiler-hydra/body
  "r" 'cat-org-roam-prefix
  "s" #'rg-menu
  "t" 'cat-toggle-prefix
  "w" #'cat-workspace/body)

(defvar-keymap cat-escape-map
  :keymap esc-map
  "p" #'cat-cape-prefix)
