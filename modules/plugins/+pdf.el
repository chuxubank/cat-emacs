;; -*- lexical-binding: t; -*-

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :bind
  (:map pdf-view-mode-map
        ("v" . +pdf-keyboard-select-region)
        ("C-S-l" . #'pdf-view-center-in-window))
  :custom
  (pdf-view-use-scaling t)
  :hook
  (pdf-view-mode . pdf-view-themed-minor-mode)
  (pdf-view-mode . pdf-view-roll-minor-mode)
  :config
  (pdf-loader-install :no-query))

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link)
  :custom
  (org-pdftools-use-isearch-link t))

;; https://github.com/dalanicolai/dala-emacs-lisp/blob/master/pdf-keyboard-highlight.el#L79
(defun +pdf-keyboard-select-region (&optional all-pages)
  "Select pdf region by search, search ALL-PAGES if have argument."
  (interactive "P")
  (pdf-view-deactivate-region)
  (let* ((pages (if all-pages nil (pdf-view-current-page)))
         (candidates (mapcar (lambda (x)
                               (list (cdar (cdr x))
                                     (cdar x)
                                     (cdar (cddr x))))
                             (pdf-info-search-regexp (read-string "Regexp: ") pages)))
         (page-edges-list (alist-get (completing-read "Select correct context: " candidates)
                                     candidates nil nil 'equal))
         (page (car page-edges-list))
         (edges-list (cadr page-edges-list))
         (edges (append (cl-subseq (car edges-list) 0 2) (cl-subseq (car (last edges-list)) 2 4))))
    (pdf-view-goto-page page)
    (setq pdf-view-active-region (list page edges))
    (pdf-view--push-mark)
    (pdf-view-display-region)))
