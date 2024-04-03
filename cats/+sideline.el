;; -*- lexical-binding: t; -*-

(use-package sideline
  :delight
  :hook (after-init . global-sideline-mode)
  :custom
  (sideline-display-backend-name t)
  :config
  (defun sideline--string-pixel-width (string)
    "Return the width of STRING in pixels.

Sideline's own version of `string-pixel-width'
which respect the `face-remapping-alist'."
    (if (zerop (length string))
        0
      (let ((remapping-alist face-remapping-alist))
        ;; Prevent use original buffer name for minimal side-effects
        (with-current-buffer (get-buffer-create " *sideline-string-pixel-width*")
          (setq-local display-line-numbers nil)
          (delete-region (point-min) (point-max))
          (setq-local face-remapping-alist remapping-alist)
          (insert string)
          (car (buffer-text-pixel-size nil nil t))))))

  (defun sideline--align-right (str offset)
    "Align sideline STR from the right of the window.

Argument OFFSET is additional calculation from the right alignment."
    (let ((graphic-p (display-graphic-p))
          (fringes (window-fringes)))
      (list (+
             ;; If the sideline text is displayed without at least 1 pixel gap from the right fringe and
             ;; overflow-newline-into-fringe is not true, emacs will line wrap it.
             (if (and graphic-p
                      (> (nth 1 fringes) 0)
                      (not overflow-newline-into-fringe))
                 1
               0)
             (* (window-font-width)
                (+ offset (if graphic-p
                              ;; If right fringe deactivated add 1 offset
                              (if (= 0 (nth 1 fringes)) 1 0)
                            1)))
             (sideline--string-pixel-width str))))))

(use-package sideline-flycheck
  :hook (flycheck-mode . sideline-flycheck-setup)
  :config
  (add-to-list 'sideline-backends-right #'sideline-flycheck))

(use-package sideline-flymake
  :demand t
  :after flymake
  :config
  (add-to-list 'sideline-backends-right #'sideline-flymake))

(use-package sideline-blame
  :demand t
  :after sideline
  :custom
  (sideline-blame-commit-format " %s")
  :config
  (add-to-list 'sideline-backends-right #'sideline-blame))
