;; -*- lexical-binding: t; -*-

(use-package meow)

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty
        meow-keypad-leader-dispatch "C-c"
        meow-expand-exclude-mode-list nil
        meow-replace-state-name-list
        '((normal . "🅝")
          (beacon . "🅑")
          (insert . "🅘")
          (motion . "🅜")
          (keypad . "🅚")))
  (meow-leader-define-key
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   '("u" . meow-universal-argument))
  (meow-motion-overwrite-define-key
   '("<escape>" . ESC-prefix))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("C" . meow-change-save)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-line)
   '("f" . meow-find)
   '("F" . meow-find-expand)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-join)
   '("J" . meow-start-kmacro-or-insert-counter)
   '("k" . meow-kill)
   '("K" . meow-end-or-call-kmacro)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-prev)
   '("P" . meow-prev-expand)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-search)
   '("S" . meow-pop-search)
   '("t" . meow-till)
   '("T" . meow-till-expand)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . meow-save)
   '("X" . meow-sync-grab)
   '("y" . meow-yank)
   '("Y" . meow-yank-pop)
   '("z" . meow-pop-selection)
   '("Z" . meow-pop-all-selection)
   '("%" . meow-query-replace)
   '("C-%" . meow-query-replace-regexp)
   '("'" . repeat)
   '("<escape>" . ESC-prefix)
   (when (package-installed-p 'embark)
     '(">" . embark-act))
   (when (package-installed-p 'avy)
     '(":" . avy-goto-char-timer)))
  (define-keymap
    :keymap meow-insert-state-keymap
    "C-g" #'meow-insert-exit
    "<escape>" #'ESC-prefix))

(meow-setup)
(meow-setup-line-number)
(+add-to-list-multi 'meow-mode-state-list
                    '(diary-mode . normal)
                    '(help-mode . motion)
                    '(telega-root-mode . motion)
                    '(osx-dictionary-mode . motion)
                    '(eshell-mode . insert)
                    '(term-mode . insert)
                    '(comint-mode . insert))

(when (featurep 'nano-modeline)
  (defun +nano-modeline-meow-indicator (args)
    (cl-destructuring-bind (left right face-prefix) args
      (let* ((face (nano-modeline--base-face face-prefix))
             (left (append left '((meow-indicator)))))
        (dolist (meow-face meow-indicator-face-alist)
          (let ((meow-face (cdr meow-face)))
            (eval `(face-spec-set ',meow-face '((t (:inherit ,face)))))))
        (list left right face-prefix))))

  (defun +meow-setup-nano-modeline ()
    "Toggle meow-indicator for all buffers"
    (if meow-mode
        (advice-add #'nano-modeline--make :filter-args #'+nano-modeline-meow-indicator)
      (advice-remove #'nano-modeline--make #'+nano-modeline-meow-indicator)))

  (add-hook 'meow-global-mode-hook #'+meow-setup-nano-modeline))

(with-eval-after-load 'doom-modeline
  (defun +meow-setup-doom-modeline ()
    "Toggle meow minor mode indicator"
    (if doom-modeline-mode
        (+change-lighter 'meow-beacon-mode ""
                         'meow-keypad-mode ""
                         'meow-motion-mode ""
                         'meow-normal-mode ""
                         'meow-insert-mode "")
      (+change-lighter 'meow-beacon-mode " [B]"
                       'meow-keypad-mode " [K]"
                       'meow-motion-mode " [M]"
                       'meow-normal-mode " [N]"
                       'meow-insert-mode " [I]")))
  (add-hook 'doom-modeline-mode-hook #'+meow-setup-doom-modeline))

(meow-global-mode 1)
