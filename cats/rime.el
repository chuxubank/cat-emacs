(use-package rime
  :commands #'toggle-input-method)

(when IS-MAC
  (setq rime-librime-root "~/librime/dist"))

(when IS-MINGW64
  (setq rime-librime-root (getenv "emacs_dir")
	rime-share-data-dir (concat (getenv "emacs_dir") "/share/rime-data")))

(setq
 rime-user-data-dir "~/rime"
 rime-disable-predicates
 '(meow-normal-mode-p
   meow-motion-mode-p
   meow-keypad-mode-p
   rime-predicate-hydra-p
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
 rime-show-candidate 'sidewindow
 default-input-method "rime"
 rime-cursor "|")

(with-eval-after-load "rime"
  (define-key rime-active-mode-map [tab] 'rime-inline-ascii)
  (define-key rime-mode-map (kbd "C-`") 'rime-send-keybinding)
  (define-key rime-mode-map (kbd "M-j") 'rime-force-enable))

(when (featurep 'nano-modeline)
  (defun +nano-modeline-rime-indicator (args)
    (cl-destructuring-bind (status name primary secondary) args
      (list status
	    name
	    (concat primary " " (rime-lighter))
	    secondary)))
  (advice-add #'nano-modeline-compose :filter-args #'+nano-modeline-rime-indicator))
