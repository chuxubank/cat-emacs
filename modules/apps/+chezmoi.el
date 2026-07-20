;; -*- lexical-binding: t; -*-

(use-package chezmoi
  :vc (chezmoi :url "https://github.com/chuxubank/chezmoi.el")
  :delight " "
  :custom
  (chezmoi-auto-enable-mode nil))

(defvar cat-chezmoi-extensions-load-path
  (expand-file-name "extensions" (file-name-directory (locate-library "chezmoi"))))

(defun cat/chezmoi-mode-p ()
  "Return non-nil if `chezmoi-mode' minor mode is enabled in the current buffer."
  (bound-and-true-p chezmoi-mode))

(use-package chezmoi-dired
  :load-path cat-chezmoi-extensions-load-path
  :commands #'chezmoi-dired-add-marked-files)

(use-package chezmoi-ediff
  :load-path cat-chezmoi-extensions-load-path
  :init
  (setq age-default-identity nil
        age-default-recipient nil)
  :commands #'chezmoi-ediff)

(when (package-installed-p 'magit)
  (use-package chezmoi-magit
    :load-path cat-chezmoi-extensions-load-path
    :demand t
    :after chezmoi magit
    :commands #'chezmoi-magit-status))
