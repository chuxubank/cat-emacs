;; -*- lexical-binding: t; -*-

(use-package rime
  :commands #'toggle-input-method)

(when IS-MAC
  (setq rime-librime-root "~/librime/dist"))

(setq
 rime-user-data-dir "~/rime"
 rime-disable-predicates
 '(rime-predicate-hydra-p
   rime-predicate-ace-window-p
   rime-predicate-prog-in-code-p
   rime-predicate-org-latex-mode-p
   rime-predicate-org-in-src-block-p
   rime-predicate-after-ascii-char-p
   rime-predicate-tex-math-or-command-p
   rime-predicate-punctuation-line-begin-p
   rime-predicate-punctuation-after-space-cc-p)
 rime-inline-predicates
 '(rime-predicate-space-after-cc-p
   rime-predicate-current-uppercase-letter-p)
 rime-translate-keybindings
 '("<C-delete>" "C-f" "C-b" "C-n" "C-p" "C-g")
 rime-show-candidate 'posframe
 default-input-method "rime"
 rime-cursor "|")

(with-eval-after-load 'rime
  (define-key rime-active-mode-map [tab] 'rime-inline-ascii)
  (define-key rime-mode-map (kbd "C-`") 'rime-send-keybinding)
  (define-key rime-mode-map (kbd "M-j") 'rime-force-enable))

(when (featurep 'meow)
  (+add-to-list-multi 'rime-disable-predicates
		      #'meow-normal-mode-p
		      #'meow-motion-mode-p
		      #'meow-keypad-mode-p))

(when (featurep 'nano-modeline)
  (defun +rime-ligher-dynamic-spacer ()
    (unless (string-empty-p (rime-lighter)) " "))

  (defun +nano-modeline-rime-indicator (args)
    (cl-destructuring-bind (left right face-prefix) args
      (list (append left '((+rime-ligher-dynamic-spacer) (rime-lighter)))
	    right
	    face-prefix)))
  (advice-add #'nano-modeline--make :filter-args #'+nano-modeline-rime-indicator))
