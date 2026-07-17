;;; poly-any-go-template.el --- Go templates in any host mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Misaka

;; Author: Misaka <chuxubank@qq.com>
;; Maintainer: Misaka <chuxubank@qq.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (polymode "0.2") (go-template-ts-mode "0.1.0"))
;; Keywords: polymode, go, templates, tree-sitter
;; URL: https://github.com/chuxubank/cat-emacs

;;; Commentary:

;; Edit Go template actions with `go-template-ts-mode' while preserving the
;; major mode inferred from the extension before `.gotmpl'.  For example,
;; `deployment.yaml.gotmpl' uses the YAML mode as its host mode.

;;; Code:

(require 'polymode)
(require 'go-template-ts-mode)

(defun poly-any-go-template--head-matcher (direction)
  "Find a template action start in DIRECTION and return a zero-width match."
  (let ((found (if (< direction 0)
                   (re-search-backward "{{-?" nil t)
                 (re-search-forward "{{-?" nil t))))
    (when found
      (cons (match-beginning 0) (match-beginning 0)))))

(defun poly-any-go-template--tail-matcher (_direction)
  "Find the current template action end and return a zero-width match."
  (when (re-search-forward "-?}}" nil t)
    (cons (match-end 0) (match-end 0))))

(define-innermode poly-go-template-innermode
  :mode #'go-template-ts-mode
  :head-matcher #'poly-any-go-template--head-matcher
  :tail-matcher #'poly-any-go-template--tail-matcher
  :head-adjust-face nil)

(defun poly-any-go-template--get-major-mode-for-file (filename)
  "Return the major mode selected for FILENAME."
  (when filename
    (ignore-errors
      (with-temp-buffer
        (set-visited-file-name filename t t)
        (set-auto-mode)
        (unless (eq major-mode 'fundamental-mode)
          major-mode)))))

;;;###autoload
(defun poly-any-go-template-mode ()
  "Use a filename-derived host mode around Go template actions.
For example, `deployment.yaml.gotmpl' uses the mode selected for
`deployment.yaml' as its host mode."
  (interactive)
  (let* ((base-filename (when buffer-file-name
                          (file-name-sans-extension buffer-file-name)))
         (host-major-mode
          (or (poly-any-go-template--get-major-mode-for-file base-filename)
              'text-mode))
         (host-mode-symbol
          (intern (format "poly-%s-go-template-hostmode" host-major-mode)))
         (polymode-symbol
          (intern (format "poly-%s-go-template-mode"
                          (string-remove-suffix
                           "-mode" (symbol-name host-major-mode))))))
    (unless (fboundp host-mode-symbol)
      (eval `(define-hostmode ,host-mode-symbol :mode ',host-major-mode) t))
    (unless (fboundp polymode-symbol)
      (eval `(define-polymode ,polymode-symbol
               :hostmode ',host-mode-symbol
               :innermodes '(poly-go-template-innermode)
               :lighter " GoTpl") t))
    (funcall polymode-symbol)))

;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.[^./]+\\.gotmpl\\'" . poly-any-go-template-mode))

(provide 'poly-any-go-template)
;;; poly-any-go-template.el ends here
