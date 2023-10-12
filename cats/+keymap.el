;; -*- lexical-binding: t; -*-

(defvar-keymap cat-toggle-map
  :doc "Keymap for toggle commands."
  :name "Cat Toggle"
  :prefix 'cat-toggle-prefix
  "d" 'toggle-debug-on-error
  "f" 'display-fill-column-indicator-mode
  "l" 'display-line-numbers-mode
  "s" 'whitespace-mode
  "t" 'toggle-truncate-lines
  "v" 'visual-line-mode
  "V" 'view-mode
  "w" 'toggle-word-wrap)

(defvar-keymap cat-plugin-map
  :doc "Keymap for plugins."
  :name "Cat Plugin"
  :prefix 'cat-plugin-prefix
  "a" 'anki-vocabulary
  "b" 'bing-dict-brief
  "c" 'chezmoi-prefix
  "d" 'deft
  "g" 'igist-dispatch
  "l" 'elogcat
  "m" 'mu4e
  "o" 'osx-dictionary-search-pointer
  "O" 'osx-dictionary-search-input
  "p" 'pass
  "s" 'rg-menu
  "r" 'elfeed
  "R" 'magit-list-repositories
  "t" 'telega-prefix
  "w" 'wl
  "v" 'vundo)

(defvar-keymap cat-map
  :keymap mode-specific-map
  "d" org-roam-dailies-prefix
  "f" cat-file-prefix
  "j" org-jira-prefix
  "p" cat-plugin-prefix
  "r" org-roam-prefix
  "t" cat-toggle-prefix)
