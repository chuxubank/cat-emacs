;; -*- lexical-binding: t; -*-

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :custom
  (pdf-view-use-scaling t)
  :hook
  (pdf-view-mode . pdf-view-themed-minor-mode)
  :config
  (pdf-loader-install :no-query))

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link)
  :custom
  (org-pdftools-use-isearch-link t))

(defun +pdf-keyboard-select-region (&optional all-pages-p)
  "Ref: https://github.com/dalanicolai/dala-emacs-lisp/blob/9662aa2ab993157e6e7587385d27c48ed50a1a50/pdf-keyboard-highlight.el#L79"
  (interactive "P")
  (pdf-view-deactivate-region)
  (let* ((pages (if all-pages-p nil (pdf-view-current-page)))
         (candidates (mapcar (lambda (x)
                               (list (cdar (cdr x))
                                     (cdar x)
                                     (cdar (cddr x))))
                             (pdf-info-search-regexp (read-string "Regexp: ") pages)))
         (page-edges-list (alist-get (completing-read "Select correct context: " candidates)
                                     candidates nil nil 'equal))
         (edges-list (cadr page-edges-list))
         (edges (append (cl-subseq (car edges-list) 0 2) (cl-subseq (car (last edges-list)) 2 4))))
    (pdf-view-goto-page (car page-edges-list))
    (setq pdf-view-active-region (list edges))
    (pdf-view--push-mark)
    (pdf-view-display-region)))

(with-eval-after-load 'pdf-view
  (define-key pdf-view-mode-map (kbd "v") #'+pdf-keyboard-select-region))
