;; -*- lexical-binding: t; -*-

(use-package chezmoi-mode
  :vc (:url "https://github.com/chuxubank/chezmoi-mode")
  :delight (chezmoi-mode (:eval (+with-icon "nf-cod-home" " ")))
  :hook
  (chezmoi-template-mode . poly-any-go-template-mode)
  :config
  (add-to-list
   'poly-any-go-template-extra-file-name-rules
   #'chezmoi-template-source-file-p)
  (add-to-list
   'poly-any-template-host-filename-functions
   #'chezmoi-template-normalize-host-filename))

(defun cat/chezmoi-mode-p ()
  "Return non-nil if `chezmoi-mode' minor mode is enabled in the current buffer."
  (bound-and-true-p chezmoi-mode))
