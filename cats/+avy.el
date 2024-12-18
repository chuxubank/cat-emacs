;; -*- lexical-binding: t; -*-

(use-package avy
  :bind
  ("C-:" . avy-goto-char-timer)
  (:map isearch-mode-map
        ("M-j" . avy-isearch))
  :config
  (when (package-installed-p 'embark)
    (defun avy-action-embark (pt)
      (unwind-protect
          (save-excursion
            (goto-char pt)
            (embark-act))
        (select-window
         (cdr (ring-ref avy-ring 0))))
      t)
    (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark)))
