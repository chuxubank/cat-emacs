;; -*- lexical-binding: t; -*-

(use-package vertico)

(vertico-mode 1)

(setq vertico-cycle t)

(defun basic-remote-try-completion (string table pred point)
  (and (vertico--remote-p string)
       (completion-basic-try-completion string table pred point)))
(defun basic-remote-all-completions (string table pred point)
  (and (vertico--remote-p string)
       (completion-basic-all-completions string table pred point)))

(add-to-list 'completion-styles-alist '(basic-remote basic-remote-try-completion basic-remote-all-completions "Basic remote completion style."))
