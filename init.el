;; -*- lexical-binding: t; -*-

(cat-benchmark 'beg)

;;; load-path
(let ((default-directory (expand-file-name "elisp" user-emacs-directory)))
  (when (file-directory-p default-directory)
    (add-to-list 'load-path default-directory)
    (normal-top-level-add-subdirs-to-load-path)))

;;; core
(require 'cat-core (expand-file-name "core/core" user-emacs-directory))

;;; ui
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; enable
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'list-threads 'disabled nil)
(put 'magit-clean 'disabled nil)

(cat-benchmark 'end)
