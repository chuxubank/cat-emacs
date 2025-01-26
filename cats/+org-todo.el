;; -*- lexical-binding: t; -*-

(use-package org-agenda
  :ensure nil
  :custom
  (org-agenda-file-regexp "\\`[^.].*\\.org\\(\\.gpg\\)?\\'")
  (org-agenda-include-diary t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-deadline-prewarning-if-scheduled t)
  (org-agenda-prefix-format '((agenda . " %i %-20:c%?-12t% s")
                              (todo   . " %i %-20:c")
                              (tags   . " %i %-20:c")
                              (search . " %i %-20:c"))))

(use-package calendar
  :ensure nil
  :custom
  (diary-file (expand-file-name "diary" cat-org-directory)))

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
  (defun org-agenda--count (list &optional type)
    "Count the number of entries in this block for `org-agenda-count'.

Intended as (temporary) :before advice for the function
`org-agenda-finalize-hook'.

This function filters out non-TODO entries before counting them."
    (let ((todo-entries
           (seq-filter (lambda (entry)
                         (get-text-property 0 'todo-state entry))
                       list)))
      (alist-put org-agenda-count--alist
                 org-agenda-count--block
                 (length todo-entries)
                 #'equal))))

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

