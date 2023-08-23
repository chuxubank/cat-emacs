;; -*- lexical-binding: t; -*-

(defmacro cat-define-key (keymap key def &optional name)
  `(when (or (keymapp ,def)
             (functionp ,def)
             (boundp ,def))
     (if ,name
         (define-key ,keymap ,key (cons ,name ,def))
       (define-key ,keymap ,key ,def))))

(defvar cat-toggle-map
  (let ((map (make-keymap)))
    (cat-define-key map "d" 'toggle-debug-on-error)
    (cat-define-key map "f" 'display-fill-column-indicator-mode)
    (cat-define-key map "l" 'display-line-numbers-mode)
    (cat-define-key map "t" 'toggle-truncate-lines)
    (cat-define-key map "v" 'visual-line-mode)
    (cat-define-key map "V" 'view-mode)
    (cat-define-key map "w" 'toggle-word-wrap)
    (cat-define-key map " " 'whitespace-mode)
    (cat-define-key map "L" 'lsp)
    map)
  "Keymap for toggle commands.")
(defalias 'cat-toggle-prefix cat-toggle-map)

(defvar cat-plugin-map
  (let ((map (make-keymap)))
    (cat-define-key map "a" 'anki-vocabulary)
    (cat-define-key map "b" 'bing-dict-brief)
    (cat-define-key map "c" 'chezmoi-prefix)
    (cat-define-key map "d" 'deft)
    (cat-define-key map "l" 'elogcat) ; logcat
    (cat-define-key map "m" 'mu4e)
    (cat-define-key map "o" 'osx-dictionary-search-pointer)
    (cat-define-key map "O" 'osx-dictionary-search-input)
    (cat-define-key map "p" 'pass)
    (cat-define-key map "s" 'rg-menu) ; search
    (cat-define-key map "r" 'elfeed) ; rss
    (cat-define-key map "R" 'magit-list-repositories)
    (cat-define-key map "t" 'telega-prefix)
    (cat-define-key map "w" 'wl)
    (cat-define-key map "v" 'vundo)
    map)
  "Keymap for plugins.")
(defalias 'cat-plugin-prefix cat-plugin-map)

(cat-define-key mode-specific-map "d" 'org-roam-dailies-prefix)
(cat-define-key mode-specific-map "f" 'cat-file-prefix)
(cat-define-key mode-specific-map "j" 'org-jira-prefix)
(cat-define-key mode-specific-map "p" 'cat-plugin-prefix)
(cat-define-key mode-specific-map "r" 'org-roam-prefix)
(cat-define-key mode-specific-map "t" 'cat-toggle-prefix)
