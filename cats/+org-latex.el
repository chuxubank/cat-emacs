;; -*- lexical-binding: t; -*-

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
      org-preview-latex-default-process 'dvisvgm
      org-preview-latex-process-alist
      '((dvisvgm :programs ("xelatex" "dvisvgm")
                 :description "xdv > svg"
                 :message "you need to install the programs: xelatex and dvisvgm."
                 :image-input-type "xdv"
                 :image-output-type "svg"
                 :image-size-adjust (2.5 . 1.5)
                 :latex-compiler ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
                 :image-converter ("dvisvgm %f -n -b min -c %S -o %O"))
        (imagemagick :programs("xelatex" "convert")
                     :description "pdf > png"
                     :message "you need to install the programs: xelatex and imagemagick."
                     :image-input-type "pdf"
                     :image-output-type "png"
                     :image-size-adjust (1.0 . 1.0)
                     :latex-compiler ("xelatex -interaction nonstopmode -output-directory %o %f")
                     :image-converter ("convert -density %D -trim -antialias %f -quality 100 %O"))))

(with-eval-after-load 'org
  (setq org-format-latex-header
        (replace-regexp-in-string
         (regexp-quote "\\documentclass{article}")
         "\\documentclass[dvisvgm]{article}"
         org-format-latex-header t t)))

(defun +org-src-redisplay-latex-preview ()
  (when (eq major-mode 'latex-mode)
    (with-current-buffer (org-src-source-buffer)
      (org-latex-preview))))

(advice-add #'org-edit-src-save :after #'+org-src-redisplay-latex-preview)

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

(add-hook 'cat-theme-refresh-hook #'+org-redisplay-all-latex-preview)

(defun cat-org-preview-clear-cache ()
  (interactive)
  (delete-directory org-preview-latex-image-directory t)
  (message "org-preview-latex-image-directory cleared"))
