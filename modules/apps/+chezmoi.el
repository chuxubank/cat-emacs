;; -*- lexical-binding: t; -*-

(use-package chezmoi
  :vc (chezmoi :url "https://github.com/chuxubank/chezmoi.el")
  :demand t)

(defvar cat-chezmoi-extensions-load-path
  (expand-file-name "extensions" (file-name-directory (locate-library "chezmoi"))))

(defun cat/chezmoi-mode-p ()
  "Return non-nil if `chezmoi-mode' minor mode is enabled in the current buffer."
  (bound-and-true-p chezmoi-mode))

(when (package-installed-p 'company)
  (use-package chezmoi-company
    :load-path cat-chezmoi-extensions-load-path
    :demand t
    :after chezmoi company
    :config
    (defun cat/add-or-remove-chezmoi-company-backend ()
      (if chezmoi-mode
          (setq-local company-backends
                      (cons 'chezmoi-company-backend
                            (remove 'chezmoi-company-backend company-backends)))
        (setq-local company-backends
                    (remove 'chezmoi-company-backend company-backends))))
    (add-hook 'chezmoi-mode-hook #'cat/add-or-remove-chezmoi-company-backend)))

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

(when (package-installed-p 'cape)
  (defun cat/chezmoi-cape-setup ()
    "Use Chezmoi completion only in buffers with `chezmoi-mode'."
    (if chezmoi-mode
        (add-hook 'completion-at-point-functions #'chezmoi-capf nil t)
      (remove-hook 'completion-at-point-functions #'chezmoi-capf t)))

  (use-package chezmoi-cape
    :load-path cat-chezmoi-extensions-load-path
    :demand t
    :after chezmoi cape
    :config
    (add-hook 'chezmoi-mode-hook #'cat/chezmoi-cape-setup)))
