;; -*- lexical-binding: t; -*-

(use-package org-agenda
  :ensure nil
  :bind
  (:map org-agenda-mode-map
        ("W" . #'org-todo-with-date)
        ("Y" . #'org-todo-yesterday))
  :custom
  (org-agenda-file-regexp "\\`[^.].*\\.org\\(\\.gpg\\)?\\'")
  (org-agenda-include-diary t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-deadline-prewarning-if-scheduled t)
  (org-agenda-prefix-format '((agenda . " %i %-20:c%?-12t% s")
                              (todo   . " %i %-20:c")
                              (tags   . " %i %-20:c")
                              (search . " %i %-20:c")))
  (org-agenda-diary-file (expand-file-name "diary.org" cat-org-directory)))

(defun cat-filter-todo-entries (args)
  "Filter out non-TODO entries from the ARGS for `org-agenda--count'."
  (let* ((list (car args))
         (filtered-list (seq-filter (lambda (entry)
                                      (get-text-property 0 'todo-state entry))
                                    list)))
    (list filtered-list (cadr args))))

(use-package org-agenda-count
  :vc (org-agenda-count
       :url "https://github.com/sid-kurias/org-agenda-count"
       :rev :newest)
  :demand t
  :after org
  :custom
  (org-agenda-custom-commands
   '(("n" "Agenda and all TODOs"
      ((agenda
        ""
        ((org-agenda-span 'day)
         (org-agenda-overriding-header
          (format "Agenda [%s]" (org-agenda-count)))))
       (alltodo
        ""
        ((org-agenda-overriding-header
          (format "All TODOs [%s]" (org-agenda-count "alltodo")))))))))
  :config
  (advice-add 'org-agenda--count :filter-args #'cat-filter-todo-entries))

(use-package org-edna
  :delight " "
  :hook (org-mode . org-edna-mode)
  :custom
  (org-edna-finder-use-cache t))

(defun org-todo-with-date (&optional arg)
  "Like `org-todo' but with given date.

Ref: https://stackoverflow.com/a/28130043
Ref: https://emacs.stackexchange.com/a/57749"
  (interactive "P")
  (cl-letf* ((org-read-date-prefer-future nil)
             (my-current-time (org-read-date t t nil "when:" nil nil nil))
             ((symbol-function #'current-time)
              #'(lambda () my-current-time))
             ((symbol-function #'org-today)
              #'(lambda () (time-to-days my-current-time)))
             ((symbol-function #'org-current-effective-time)
              #'(lambda () my-current-time)))
    (if (eq major-mode 'org-agenda-mode)
        (org-agenda-todo arg)
      (org-todo arg))))

