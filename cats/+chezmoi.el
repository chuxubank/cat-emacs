;; -*- lexical-binding: t; -*-

(use-package chezmoi
  :commands (chezmoi-find chezmoi-dired-add-marked-files)
  :mode ("\\dot_\\'" . chezmoi-mode))

(use-package chezmoi-company
  :vc (:url "https://github.com/tuh8888/chezmoi.el"
            :lisp-dir "extensions/")
  :when (package-installed-p 'company)
  :after (chezmoi company)
  :config
  (defun +add-or-remove-chezmoi-company-backend ()
    (if chezmoi-mode
        (add-to-list 'company-backends 'chezmoi-company-backend)
      (setq company-backends (delete 'chezmoi-company-backend company-backends))))
  (add-hook 'chezmoi-mode-hook #'+add-or-remove-chezmoi-company-backend))

(use-package chezmoi-dired
  :vc (:url "https://github.com/tuh8888/chezmoi.el"
            :lisp-dir "extensions/")
  :after chezmoi)

(use-package chezmoi-ediff
  :vc (:url "https://github.com/tuh8888/chezmoi.el"
            :lisp-dir "extensions/")
  :after chezmoi)

(use-package chezmoi-magit
  :vc (:url "https://github.com/tuh8888/chezmoi.el"
            :lisp-dir "extensions/")
  :after (chezmoi magit))

(use-package chezmoi-cape
  :vc (:url "https://github.com/tuh8888/chezmoi.el"
            :lisp-dir "extensions/")
  :when (package-installed-p 'cape)
  :after (chezmoi cape)
  :config
  (add-to-list 'completion-at-point-functions #'chezmoi-capf))

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
  "t" #'chezmoi-template-buffer-display)
