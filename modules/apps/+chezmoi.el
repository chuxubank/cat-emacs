;; -*- lexical-binding: t; -*-

(defun cat/chezmoi-template-mode-setup ()
  "Select a pure or host-composed mode for a Chezmoi template."
  (cond ((or (eq major-mode 'go-template-ts-mode)
             (bound-and-true-p polymode-mode)))
        ((cat/chezmoi-template-host-mode-p buffer-file-name)
         (poly-any-go-template-mode))
        (t
         (go-template-ts-mode))))

(defun cat/chezmoi-template-host-mode-p (filename)
  "Return non-nil when FILENAME is a typed Chezmoi template."
  (when (chezmoi-template-source-file-p filename)
    (let* ((template-suffix-p
            (string-match-p "\\.\\(?:gotmpl\\|tmpl\\)\\'" filename))
           (host-filename
            (if template-suffix-p
                (file-name-sans-extension filename)
              filename)))
      (poly-any-template-host-mode-for-file
       (chezmoi-template-normalize-host-filename host-filename)))))

(use-package chezmoi
  :vc (chezmoi :url "https://github.com/chuxubank/chezmoi.el")
  :delight " "
  :custom
  (chezmoi-auto-enable-mode nil)
  :hook
  (chezmoi-template-mode . cat/chezmoi-template-mode-setup))

(use-package poly-any-go-template
  :after chezmoi
  :init
  (setq poly-any-go-template-extra-file-name-rules
        '(cat/chezmoi-template-host-mode-p))
  :config
  (add-hook 'poly-any-template-host-filename-functions
            #'chezmoi-template-normalize-host-filename))

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
