;; -*- lexical-binding: t; -*-

(defun cat/chezmoi-template-mode-setup ()
  "Select a pure or host-composed mode for a Chezmoi template."
  (unless (bound-and-true-p polymode-mode)
    (poly-any-go-template-mode)))

(use-package chezmoi-mode
  :vc (:url "https://github.com/chuxubank/chezmoi-mode")
  :delight " "
  :hook
  (chezmoi-template-mode . cat/chezmoi-template-mode-setup))

(use-package poly-any-go-template
  :init
  (setq poly-any-go-template-extra-file-name-rules
        '(chezmoi-template-source-file-p))
  (add-hook 'poly-any-template-host-filename-functions
            #'chezmoi-template-normalize-host-filename))

(defun cat/chezmoi-mode-p ()
  "Return non-nil if `chezmoi-mode' minor mode is enabled in the current buffer."
  (bound-and-true-p chezmoi-mode))
