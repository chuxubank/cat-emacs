;; -*- lexical-binding: t; -*-

(use-package chezmoi
  :commands (chezmoi-find chezmoi-dired-add-marked-files)
  :mode ("\\dot_\\'" . chezmoi-mode))

(use-package chezmoi-company
  :straight (chezmoi-company :host github :repo "tuh8888/chezmoi.el" :files ("extensions/chezmoi-company.el"))
  :after (chezmoi company)
  :config
  (defun +add-or-remove-chezmoi-company-backend ()
    (if chezmoi-mode
        (add-to-list 'company-backends 'chezmoi-company-backend)
      (setq company-backends (delete 'chezmoi-company-backend company-backends))))
  (add-hook 'chezmoi-mode-hook #'+add-or-remove-chezmoi-company-backend))

(use-package chezmoi-dired
  :straight (chezmoi-dired :host github :repo "tuh8888/chezmoi.el" :files ("extensions/chezmoi-dired.el"))
  :after chezmoi)

(use-package chezmoi-ediff
  :straight (chezmoi-ediff :host github :repo "tuh8888/chezmoi.el" :files ("extensions/chezmoi-ediff.el"))
  :after chezmoi)

(use-package chezmoi-magit
  :straight (chezmoi-magit :host github :repo "tuh8888/chezmoi.el" :files ("extensions/chezmoi-magit.el"))
  :after (chezmoi magit))

(use-package chezmoi-cape
  :straight (chezmoi-cape :host github :repo "tuh8888/chezmoi.el" :files ("extensions/chezmoi-cape.el"))
  :after (chezmoi cape)
  :config
  (add-to-list 'completion-at-point-functions #'chezmoi-capf))

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
