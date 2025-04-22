;; -*- lexical-binding: t; -*-

(cat-benchmark 'beg)

;;; custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(defgroup cat-emacs nil
  "A lightweight Emacs configuration works on Linux, macOS and Windows."
  :group 'Emacs
  :version "29.3")

(defcustom cat-org-directory
  (or (getenv "ORG_DIR") "~/Developer/Personal/org/")
  "Filename of the org folder.
See `org-directory'."
  :type 'directory
  :group 'cat-emacs)

(defcustom cat-blog-directory
  (or (getenv "BLOG_DIR") "~/Developer/Personal/blog/")
  "Filename of the blog folder.
See `hugoista-site-dir' and `easy-hugo-basedir'."
  :type 'directory
  :group 'cat-emacs)

(defcustom cat-pass-directory
  (or (getenv "PASSWORD_STORE_DIR") "~/.password-store")
  "Filename of the password-store folder.
See `auth-source-pass-filename'."
  :type 'directory
  :group 'cat-emacs)

(defcustom cat-default-csl-styles-dir
  (or (getenv "DEFAULT_CSL_STYLES_DIR") "~/Zotero/styles/")
  "Filename of the default csl styles folder.
See `org-cite-csl-styles-dir'."
  :type 'directory
  :group 'cat-emacs)

(defcustom cat-codeium-dir
  (or (getenv "CODEIUM_DIR") "~/.codeium/")
  "Filename of the codeium folder."
  :type 'directory
  :group 'cat-emacs)

;;; packages
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

;;; modules
(defmacro cat! (filename &optional path noerror)
  "Load a module in cats by default"
  (let* ((path (or path
                   (expand-file-name "cats" user-emacs-directory)
                   (error "Could not detect path to look for '%s' in"
                          filename)))
         (file (if path
                   `(expand-file-name ,filename ,path)
                 filename)))
    `(condition-case-unless-debug err
         (let (file-name-handler-alist)
           (cat-benchmark 'beg ,file)
           (load ,file ,noerror 'nomessage))
       (error
        (message "ERROR: %S when loading file: %s\nBacktrace:\n%s"
                 err
                 (abbreviate-file-name ,file)
                 (with-output-to-string (backtrace)))))))

(load (concat user-emacs-directory "config") nil 'nomessage)

;;; ui
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; enable
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'list-threads 'disabled nil)
(put 'magit-clean 'disabled nil)

(cat-benchmark 'end)
