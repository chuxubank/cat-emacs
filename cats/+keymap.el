;; -*- lexical-binding: t; -*-

(defmacro cat-define-key (keymap key def &optional name)
  `(when (or (keymapp ,def)
	     (functionp ,def))
     (if ,name
	 (define-key ,keymap ,key (cons ,name ,def))
       (define-key ,keymap ,key ,def))))

(defvar cat-toggle-map
  (let ((map (make-keymap)))
    (define-key map "d" 'toggle-debug-on-error)
    (define-key map "f" 'display-fill-column-indicator-mode)
    (define-key map "l" 'display-line-numbers-mode)
    (define-key map "t" 'toggle-truncate-lines)
    (define-key map "v" 'visual-line-mode)
    (define-key map "V" 'view-mode)
    (define-key map "w" 'toggle-word-wrap)
    (define-key map " " 'whitespace-mode)
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
    (cat-define-key map "t" telega-prefix-map "telega")
    (cat-define-key map "l" #'magit-list-repositories)
    (cat-define-key map "m" #'mu4e)
    map)
  "Keymap for plugins.")

(cat-define-key mode-specific-map "f" cat-file-map "cat-file-map")
(cat-define-key mode-specific-map "p" cat-plugin-map "cat-plugin-map")
(cat-define-key mode-specific-map "t" cat-toggle-map "cat-toggle-map")
