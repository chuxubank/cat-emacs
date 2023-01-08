;; -*- lexical-binding: t; -*-

(use-package meow)

(meow-global-mode 1)

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty
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
   '("-" . negative-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   '("u" . meow-universal-argument)
   '("o" . cat-other-window-frame))
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
   '("&" . meow-query-replace)
   '("%" . meow-query-replace-regexp)
   '("<escape>" . meow-last-buffer)))

(with-eval-after-load 'meow
  (meow-setup)
  (meow-setup-line-number)
  (define-key meow-insert-state-keymap (kbd "C-g") #'meow-insert-exit)
  (add-to-list 'meow-mode-state-list '(bibtex-mode . normal))
  (add-to-list 'meow-mode-state-list '(diary-mode . normal))
  (add-to-list 'meow-mode-state-list '(telega-root-mode . motion)))

(defun cat-manual-motion-mode ()
  (meow-motion-mode 'toggle)
  (meow-normal-mode 'toggle)
  (message "Toggled the meow motion mode"))

(add-hook 'shell-mode-hook #'meow-insert)
(add-hook 'comint-mode-hook #'meow-insert)

(add-hook 'view-mode-hook #'cat-manual-motion-mode)

(when (featurep 'nano-modeline)
  (defun +nano-modeline-meow-indicator (args)
    (cl-destructuring-bind (icon name primary secondary) args
      (list icon
	    name
	    (concat primary (meow-indicator))
	    secondary)))
  (advice-add #'nano-modeline-render :filter-args #'+nano-modeline-meow-indicator))
