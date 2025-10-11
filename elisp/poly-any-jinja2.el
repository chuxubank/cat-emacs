;;; poly-any-jinja2.el --- A universal polymode for Jinja2 templates -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2025  Misaka

;; Author: Misaka <chuxubank@qq.com>
;; Maintainer: Misaka <chuxubank@qq.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (polymode "0.2") (poly-ansible "0.5.2"))
;; Keywords: polymode, jinja2, templates
;; Homepage: https://github.com/chuxubank/poly-any-jinja2.el

;;; Commentary:
;;
;; This Emacs Lisp file defines a polymode for editing Jinja2 templates
;; embedded within various host file types.  It automatically detects the
;; appropriate host major mode based on the file name before the .j2 or
;; .jinja2 extension.  For example, a file named "config.json.j2"
;; will use 'json-mode' as the host mode.
;;
;; It leverages the `polymode' and `poly-jinja2' packages to provide
;; seamless editing of Jinja2 templates within different file types.
;; To use this mode, simply open a file with a .j2 or .jinja2 extension,
;; and the appropriate polymode will be activated automatically.
;; This file requires `polymode' and `poly-jinja2' to be installed.

;;; Code:

(require 'polymode)
(require 'poly-jinja2)

(defun poly-any-jinja2--get-major-mode-for-file (filename)
  "Determine the major mode for a given FILENAME.
This is done by simulating a file visit in a temporary buffer and
letting `set-auto-mode` determine the correct major mode.
Returns the major-mode symbol."
  (ignore-errors
    (with-temp-buffer
      (set-visited-file-name filename t t)
      (set-auto-mode)
      major-mode)))

;;;###autoload
(defun poly-any-jinja2-mode ()
  "A universal polymode for Jinja2 templates.
This mode automatically detects the host mode based on the file name
before the .j2 or .jinja2 extension. For example, for a file named
'config.json.j2', it will use 'json-mode' as the host mode."
  (interactive)
  (let* ((filename (buffer-file-name))
         (base-filename (when filename
                          (file-name-sans-extension filename)))
         (host-major-mode (or (poly-any-jinja2--get-major-mode-for-file base-filename)
                              'text-mode))
         ;; Generate unique, predictable names for the host and polymode functions.
         ;; e.g., 'yaml-mode' -> 'poly-yaml-mode-hostmode' and 'poly-yaml-jinja2-mode'
         (host-mode-name (format "poly-%s-hostmode" host-major-mode))
         (polymode-name (format "poly-%s-jinja2-mode"
                                (string-remove-suffix "-mode" (symbol-name host-major-mode))))
         (host-mode-symbol (intern host-mode-name))
         (polymode-symbol (intern polymode-name)))

    (unless (fboundp host-mode-symbol)
      (eval `(define-hostmode ,host-mode-symbol :mode ',host-major-mode) t))

    (unless (fboundp polymode-symbol)
      (eval `(define-polymode ,polymode-symbol
               :hostmode ',host-mode-symbol
               :innermodes '(poly-jinja2-innermode)) t))

    (funcall polymode-symbol)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.*\\.\\(j2\\|jinja2\\)\\'" . poly-any-jinja2-mode))

(provide 'poly-any-jinja2)
;;; poly-any-jinja2.el ends here
