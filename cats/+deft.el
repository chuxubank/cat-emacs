;; -*- lexical-binding: t; -*-

(use-package deft
  :config
  (setq deft-directory cat-org-roam-directory
	deft-recursive t
	deft-strip-summary-regexp
	(concat "\\("
		"[\n\t]" ;; blank
		"\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
		"\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n"
		"\\)"))
  (defun cat-deft-parse-title (file contents)
    "Parse the given FILE and CONTENTS and determine the title.
  If `deft-use-filename-as-title' is nil, the title is taken to
  be the first non-empty line of the FILE.  Else the base name of the FILE is
  used as title."
    (let ((begin (string-match "^#\\+[tT][iI][tT][lL][eE]: .*$" contents)))
      (if begin
	  (string-trim (substring contents begin (match-end 0)) "#\\+[tT][iI][tT][lL][eE]: *" "[\n\t ]+")
	(deft-base-filename file))))

  (advice-add 'deft-parse-title :override #'cat-deft-parse-title))
