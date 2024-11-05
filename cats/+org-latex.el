;; -*- lexical-binding: t; -*-

(use-package org-latex-preview
  :ensure nil
  :hook (org-mode . org-latex-preview-auto-mode)
  :custom
  (org-latex-preview-live t)
  (org-latex-preview-live-debounce 0.25)
  (org-latex-preview-auto-ignored-commands
   '(next-line
     previous-line
     mwheel-scroll
     scroll-up-command
     scroll-down-command))
  :config
  (setq org-latex-preview-preamble
        (replace-regexp-in-string
         (regexp-quote "\\documentclass{article}")
         "\\documentclass[dvisvgm]{article}"
         org-latex-preview-preamble t t)))

(setq org-latex-compiler "xelatex"
      org-latex-packages-alist
      '(("" "ctex" t ("xelatex"))
        ("" "booktabs" nil)
        ("" "enumitem" nil)
        ("" "fontspec" nil)
        ("" "svg" nil)
        ("" "pgfplots" t)
        ("" "geometry" nil)
        ("" "extarrows" t))
      org-latex-default-table-environment "longtable"
      org-preview-latex-image-directory (concat cat-cache-dir "org-latex/")
      org-preview-latex-default-process 'dvisvgm)

(defun +org-src-redisplay-latex-preview ()
  (when (eq major-mode 'latex-mode)
    (with-current-buffer (org-src-source-buffer)
      (org-latex-preview))))

;; (advice-add #'org-edit-src-save :after #'+org-src-redisplay-latex-preview)

(defun +org-redisplay-all-latex-preview ()
  "Copy from `org-revert-all-org-buffers', refresh all latex preview."
  (interactive)
  (save-excursion
    (save-window-excursion
      (dolist (b (buffer-list))
	    (when (and (with-current-buffer b (derived-mode-p 'org-mode))
		           (with-current-buffer b buffer-file-name)
                   (with-current-buffer b org-startup-with-latex-preview))
	      (pop-to-buffer-same-window b)
	      (org-latex-preview '(64))
          (org-latex-preview '(16)))))))

;; (add-hook 'cat-theme-refresh-hook #'+org-redisplay-all-latex-preview)

(defun cat-org-preview-clear-cache ()
  (interactive)
  (delete-directory org-preview-latex-image-directory t)
  (message "`%s' cleared" org-preview-latex-image-directory))
