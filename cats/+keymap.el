;; -*- lexical-binding: t; -*-

(defmacro cat-define-key (keymap key def &optional name)
  `(when (functionp ,def)
     (if ,name
	 (define-key ,keymap ,key (cons ,name ,def))
       (define-key ,keymap ,key ,def))))

(defmacro cat-define-map (keymap key def name)
  `(when (and (boundp ',def)
	      (keymapp ,def))
     (define-key ,keymap ,key (cons ,name ,def))))

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
    (cat-define-key map "L" #'lsp)
    map)
  "Keymap for toggle commands.")

(defvar cat-plugin-map
  (let ((map (make-keymap)))
    (cat-define-key map "a" #'anki-vocabulary)
    (cat-define-key map "b" #'bing-dict-brief)
    (cat-define-key map "d" #'deft)
    (cat-define-key map "e" #'elfeed)
    (cat-define-key map "p" #'pass)
    (cat-define-key map "s" #'rg-menu)
    (cat-define-key map "w" #'wl)
    (cat-define-map map "t" telega-prefix-map "telega")
    (cat-define-key map "l" #'magit-list-repositories)
    (cat-define-key map "m" #'mu4e)
    map)
  "Keymap for plugins.")

(cat-define-map mode-specific-map "f" cat-file-map "cat-file-map")
(cat-define-map mode-specific-map "p" cat-plugin-map "cat-plugin-map")
(cat-define-map mode-specific-map "r" cat-org-roam-map "cat-org-roam-map")
(cat-define-map mode-specific-map "t" cat-toggle-map "cat-toggle-map")
