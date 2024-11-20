;; -*- lexical-binding: t; -*-

(defun flycheck-display-error-messages-unless-error-list-or-sideline (errors)
  "Show messages of ERRORS unless the `sideline-flycheck' is loaded or the error list is visible."
  (unless (or (featurep 'sideline-flycheck)
              (flycheck-get-error-list-window 'current-frame))
    (flycheck-display-error-messages errors)))

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-mode-line-prefix "")
  (flycheck-indication-mode 'left-margin)
  (flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list-or-sideline))

(use-package flycheck-pos-tip
  :disabled
  :hook (flycheck-mode . flycheck-pos-tip-mode))

(use-package flycheck-posframe
  :disabled
  :hook (flycheck-mode . flycheck-posframe-mode))
