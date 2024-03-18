;; -*- lexical-binding: t; -*-

(use-package sideline
  :delight
  :hook (after-init . global-sideline-mode)
  :custom
  (sideline-display-backend-name t)
  :config
  (defun sideline--align-right (str offset)
    "Align sideline STR from the right of the window.

Argument OFFSET is additional calculation from the right alignment.
Fixed version for `buffer-face-mode'."
    (list (+
           ;; If the sideline text is displayed without at least 1 pixel gap from the right fringe and
           ;; overflow-newline-into-fringe is not true, emacs will line wrap it.
           (if (and (display-graphic-p)
                    (> (nth 1 (window-fringes)) 0)
                    (not overflow-newline-into-fringe))
               1
             0)
           (* (window-font-width)
              (+ offset
                 (if (display-graphic-p)
                     ;; If right fringe deactivated add 1 offset
                     (if (= 0 (nth 1 (window-fringes))) 1 0)
                   1)
                 (length str)))
           ))))

(use-package sideline-flycheck
  :hook (flycheck-mode . sideline-flycheck-setup)
  :config
  (add-to-list 'sideline-backends-right #'sideline-flycheck))

(use-package sideline-flymake
  :demand t
  :after flymake
  :config
  (add-to-list 'sideline-backends-right #'sideline-flymake))
