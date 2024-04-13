;; -*- lexical-binding: t; -*-

(defconst cat-banner-file (concat cat-etc-dir "meow.svg"))

(defun cat-download-banner ()
  (interactive)
  (url-copy-file "https://github.com/meow-edit/meow/raw/master/meow.svg" cat-banner-file))

(use-package dashboard
  :demand t
  :bind
  (:map dashboard-mode-map
        ("j" . nil) ("n" . dashboard-next-line)
        ("k" . nil) ("p" . dashboard-previous-line)
        ("d" . dashboard-remove-item-under))
  :init
  (unless (file-exists-p cat-banner-file)
    (cat-download-banner))
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
  (dashboard-startup-banner cat-banner-file)
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  (dashboard-heading-shorcut-format " [%s]")
  (dashboard-display-icons-p t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-icon-type 'nerd-icons)
  :config
  (dashboard-setup-startup-hook))
