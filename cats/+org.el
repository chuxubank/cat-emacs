;; -*- lexical-binding: t; -*-

(use-package org
  :defer t
  :delight
  (org-indent-mode " 󰉶")
  (org-cdlatex-mode " ")
  :bind
  (:map org-mode-map
        ("M-n" . org-next-link)
        ("M-p" . org-previous-link)
        ("C-c C-x l" . org-toggle-link-display))
  :custom
  (org-auto-align-tags nil)
  (org-hide-emphasis-markers t)
  (org-id-locations-file (expand-file-name "org-id-locations" cat-etc-dir))
  (org-startup-indented t)
  (org-special-ctrl-a/e t)
  (org-tags-column 0)
  (org-outline-path-complete-in-steps nil)
  ;; org-goto
  (org-goto-auto-isearch nil)
  ;; org-refile
  (org-refile-use-outline-path 'file)
  (org-refile-targets '((nil :maxlevel . 10)
                        (org-agenda-files :maxlevel . 3)))
  ;; org-list
  (org-list-allow-alphabetical t)
  (org-list-demote-modify-bullet '(("+" . "-")
                                   ("-" . "+")
                                   ("*" . "+")
                                   ("1." . "a.")))
  ;; org-export
  (org-export-backends '(ascii beamer html icalendar latex md odt))
  ;; image
  (org-image-actual-width 500)
  (org-display-remote-inline-images 'cache)
  ;; link
  (org-return-follows-link t)
  (org-link-abbrev-alist '(("wiki-zh" . "https://zh.wikipedia.org/wiki/%h")
                           ("wiki-en" . "https://en.wikipedia.org/wiki/%s")
                           ("github" . "https://github.com/%s")
                           ("google" . "https://goo.gle/%s")
                           ("bitbucket" . "https://bitbucket.org/%s")
                           ("bili". "https://bilibili.com/video/%s")
                           ("coursera". "https://www.coursera.org/%s")))
  ;; clock
  (org-clock-persist 'history)
  (org-clock-persist-file (expand-file-name "org-clock-save.el" cat-etc-dir))
  (org-clock-clocked-in-display 'frame-title)
  (org-timer-display 'frame-title)
  :config
  (setq org-directory cat-org-directory)
  (when IS-LINUX
    (add-to-list 'org-file-apps '("\\.x?html\\'" . "firefox %s")))
  (org-clock-persistence-insinuate)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (R . t)
     (shell . t)
     (C . t)
     (plantuml . t)
     (awk . t)))
  (require 'server)
  (require 'org-protocol))

(defun +org-redisplay-inline-images-in-babel-result-h ()
  (unless (or
           ;; ...but not while Emacs is exporting an org buffer (where
           ;; `org-display-inline-images' can be awfully slow).
           (bound-and-true-p org-export-current-backend)
           ;; ...and not while tangling org buffers (which happens in a temp
           ;; buffer where `buffer-file-name' is nil).
           (string-match-p "^ \\*temp" (buffer-name)))
    (save-excursion
      (when-let ((beg (org-babel-where-is-src-block-result))
                 (end (progn (goto-char beg) (forward-line) (org-babel-result-end))))
        (org-display-inline-images nil nil (min beg end) (max beg end))))))

(add-hook 'org-babel-after-execute-hook #'+org-redisplay-inline-images-in-babel-result-h)

(with-eval-after-load 'ob-lob
  (org-babel-lob-ingest (expand-file-name "library-of-babel.org" user-emacs-directory)))

(defun +org-buffers-refresh ()
  "Save and revert all org buffers without confirm."
  (interactive)
  (org-save-all-org-buffers)
  (+no-confirm #'org-revert-all-org-buffers)
  (org-element-update-syntax))
