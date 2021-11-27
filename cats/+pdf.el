;; -*- lexical-binding: t; -*-

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :init
  (setq pdf-view-use-scaling t)
  :config
  (pdf-tools-install))

(add-hook 'pdf-view-mode-hook (lambda () (pdf-view-themed-minor-mode 1)))

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link)
  :custom
  (org-pdftools-use-isearch-link t))

(defun pdf-keyboard-select-region (&optional all-pages-p)
  "Ref: https://github.com/dalanicolai/dala-emacs-lisp/blob/9662aa2ab993157e6e7587385d27c48ed50a1a50/pdf-keyboard-highlight.el#L79
TODO: Fix all-pages search"
  (interactive "P")
  (pdf-view-deactivate-region)
  (let* ((pages (if all-pages-p nil (pdf-view-current-page)))
	 (candidates (mapcar (lambda (x)
                               (cons (cdar (cdr x))
                                     (cdar (cddr x))))
			     (pdf-info-search-regexp (read-string "Regexp: ") pages)))
         (edges-list (alist-get (completing-read "Select correct context: " candidates)
                                candidates nil nil 'equal))
         (edges (append (cl-subseq (car edges-list) 0 2) (cl-subseq (car (last edges-list)) 2 4))))
    (setq pdf-view-active-region (list edges))
    (pdf-view--push-mark)
    (pdf-view-display-region)))

(with-eval-after-load 'pdf-view
  (define-key pdf-view-mode-map (kbd "v") #'pdf-keyboard-select-region))
