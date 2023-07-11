;; -*- lexical-binding: t; -*-

(use-package chezmoi
  :commands (chezmoi-find chezmoi-dired-add-marked-files)
  :mode ("\\dot_\\'" . chezmoi-mode)
  :config
  (require 'chezmoi-company))

(defun +add-or-remove-chezmoi-company-backend ()
  (if chezmoi-mode
      (add-to-list 'company-backends 'chezmoi-company-backend)
    (setq company-backends (delete 'chezmoi-company-backend company-backends))))

(add-hook 'chezmoi-mode-hook #'+add-or-remove-chezmoi-company-backend)

(defvar chezmoi-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" #'chezmoi-dired-add-marked-files)
    (define-key map "c" #'chezmoi-mode)
    (define-key map "d" #'chezmoi-diff)
    (define-key map "e" #'chezmoi-ediff)
    (define-key map "f" #'chezmoi-find)
    (define-key map "g" #'chezmoi-magit-status)
    (define-key map "o" #'chezmoi-open-other)
    (define-key map "s" #'chezmoi-write)
    (define-key map "t" #'chezmoi-template-buffer-display)
    map)
  "Keymap for `chezmoi' commands.")
(defalias 'chezmoi-prefix chezmoi-map)
