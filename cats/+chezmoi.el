;; -*- lexical-binding: t; -*-

(use-package chezmoi
  :commands (chezmoi-find chezmoi-dired-add-marked-files)
  :mode ("\\dot_\\'" . chezmoi-mode))

(use-package chezmoi-company
  :straight (chezmoi-company :host github :repo "tuh8888/chezmoi.el" :files ("extensions/chezmoi-company.el"))
  :when (featurep 'company)
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
  :when (featurep 'cape)
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
