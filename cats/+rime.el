;; -*- lexical-binding: t; -*-

(defcustom cat-rime-directory
  (or (getenv "RIME_DIR") "~/.local/share/rime/")
  "Filename of the rime folder."
  :type 'directory
  :group 'cat-emacs)

(use-package rime
  :commands #'toggle-input-method)

(when IS-MAC
  (setq rime-librime-root (concat cat-rime-directory "dist/")))

(setq
 rime-user-data-dir (concat cat-rime-directory "data/")
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

(with-eval-after-load 'meow
  (+add-to-list-multi 'rime-disable-predicates
                      #'meow-normal-mode-p
                      #'meow-motion-mode-p
                      #'meow-keypad-mode-p))

(with-eval-after-load 'nano-modeline
  (setq rime-title " ㄓ")
  (defun +nano-modeline-rime-indicator (args)
    (cl-destructuring-bind (left right face-prefix) args
      (let* ((face (nano-modeline--base-face face-prefix))
             (left (append left '((rime-lighter)))))
        (dolist (rime-face '(rime-indicator-face
                             rime-indicator-dim-face))
          (eval `(face-spec-set ',rime-face '((t (:inherit ,face))))))
        (list left right face-prefix))))

  (advice-add #'nano-modeline--make :filter-args #'+nano-modeline-rime-indicator))
