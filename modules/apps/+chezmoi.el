;; -*- lexical-binding: t; -*-

(defun cat/chezmoi-template-mode-setup ()
  "Select a pure or host-composed mode for a Chezmoi template."
  (unless (or (eq major-mode 'go-template-ts-mode)
              (bound-and-true-p polymode-mode))
    (poly-any-go-template-mode)))

(use-package chezmoi-mode
  :vc (:url "https://github.com/chuxubank/chezmoi-mode")
  :delight " "
  :custom
  (chezmoi-auto-enable-mode nil)
  :hook
  (chezmoi-template-mode . cat/chezmoi-template-mode-setup))

(use-package poly-any-go-template
  :after chezmoi-mode
  :init
  (setq poly-any-go-template-extra-file-name-rules
        '(chezmoi-template-source-file-p))
  :config
  (add-hook 'poly-any-template-host-filename-functions
            #'chezmoi-template-normalize-host-filename))

(defvar cat-chezmoi-extensions-load-path
  (expand-file-name "extensions" (file-name-directory
                                    (locate-library "chezmoi-mode"))))

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
    :after chezmoi-mode magit
    :commands #'chezmoi-magit-status))
