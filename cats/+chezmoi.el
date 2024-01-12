;; -*- lexical-binding: t; -*-

(use-package chezmoi
  :commands (chezmoi-dired-add-marked-files
             chezmoi-diff
             chezmoi-ediff
             chezmoi-find
             chezmoi-sync-files)
  :mode ("\\dot_\\'" . chezmoi-mode))

(when (package-installed-p 'company)
  (use-package chezmoi-company
    :vc (chezmoi-company
         :url "https://github.com/tuh8888/chezmoi.el"
         :lisp-dir "extensions/")
    :demand t
    :after chezmoi company
    :config
    (defun +add-or-remove-chezmoi-company-backend ()
      (if chezmoi-mode
          (add-to-list 'company-backends 'chezmoi-company-backend)
        (setq company-backends (delete 'chezmoi-company-backend company-backends))))
    (add-hook 'chezmoi-mode-hook #'+add-or-remove-chezmoi-company-backend)))

(use-package chezmoi-dired
  :vc (chezmoi-dired
       :url "https://github.com/tuh8888/chezmoi.el"
       :lisp-dir "extensions/")
  :demand t
  :after chezmoi)

(use-package chezmoi-ediff
  :init
  (setq age-default-identity nil
        age-default-recipient nil)
  :vc (chezmoi-ediff
       :url "https://github.com/tuh8888/chezmoi.el"
       :lisp-dir "extensions/")
  :demand t
  :after chezmoi)

(when (package-installed-p 'magit)
  (use-package chezmoi-magit
    :vc (chezmoi-magit
         :url "https://github.com/tuh8888/chezmoi.el"
         :lisp-dir "extensions/")
    :demand t
    :after chezmoi magit))

(when (package-installed-p 'cape)
  (use-package chezmoi-cape
    :vc (chezmoi-cape
         :url "https://github.com/tuh8888/chezmoi.el"
         :lisp-dir "extensions/")
    :demand t
    :after chezmoi cape
    :config
    (add-to-list 'completion-at-point-functions #'chezmoi-capf)))

(defvar-keymap chezmoi-map
  :doc "Keymap for `chezmoi' commands."
  :name "Chezmoi"
  :prefix 'chezmoi-prefix
  "a" #'chezmoi-dired-add-marked-files
  "c" #'chezmoi-mode
  "d" #'chezmoi-diff
  "e" #'chezmoi-ediff
  "f" #'chezmoi-find
  "g" #'chezmoi-magit-status
  "o" #'chezmoi-open-other
  "s" #'chezmoi-write
  "S" #'chezmoi-sync-files
  "t" #'chezmoi-template-buffer-display)
