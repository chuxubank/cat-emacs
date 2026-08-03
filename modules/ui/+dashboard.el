;; -*- lexical-binding: t; -*-

(defconst cat-logo-file (concat cat-assets-dir "logo.svg"))

(defun cat/dashboard-load-bookmarks ()
  "Load dashboard bookmarks without activating editing hooks."
  (require 'bookmark)
  (unless bookmark-bookmarks-timestamp
    (let ((existing-buffer (get-file-buffer bookmark-default-file))
          find-file-hook
          prog-mode-hook
          emacs-lisp-mode-hook
          lisp-data-mode-hook
          after-change-major-mode-hook)
      (unwind-protect
          (bookmark-maybe-load-default-file)
        (unless existing-buffer
          (when-let* ((buffer (get-file-buffer bookmark-default-file)))
            (kill-buffer buffer)))))))

(use-package dashboard
  :demand t
  :bind
  (:map dashboard-mode-map
        ("j" . nil) ("n" . dashboard-next-line)
        ("k" . nil) ("p" . dashboard-previous-line)
        ("d" . dashboard-remove-item-under))
  :custom
  (dashboard-items '((projects  . 5)
                     (recents   . 5)
                     (bookmarks . 5)
                     (registers . 5)))
  (dashboard-item-shortcuts '((recents . "f")
                              (bookmarks . "b")
                              (projects . "j")
                              (agenda . "a")
                              (registers . "r")))
  (dashboard-banner-logo-title "Cat Emacs")
  (dashboard-startup-banner (if IS-ANDROID 'official cat-logo-file))
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  (dashboard-heading-shorcut-format " [%s]")
  (dashboard-display-icons-p t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-icon-type 'nerd-icons)
  :config
  (add-hook 'dashboard-before-initialize-hook
            #'cat/dashboard-load-bookmarks)
  (dashboard-setup-startup-hook))
