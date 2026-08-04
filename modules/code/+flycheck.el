;; -*- lexical-binding: t; -*-

(defun flycheck-display-error-messages-unless-error-list-or-sideline (errors)
  "Show messages of ERRORS unless the `sideline-flycheck' is loaded or the error list is visible."
  (unless (or (featurep 'sideline-flycheck)
              (flycheck-get-error-list-window 'current-frame))
    (flycheck-display-error-messages errors)))

(defun cat/flycheck-mode-line-status (&optional status)
  "Return Flycheck STATUS as mode-line segments preserving the prefix face."
  (let* ((text (flycheck-mode-line-status-text status))
         (prefix (substring-no-properties flycheck-mode-line-prefix))
         (face (and (not (string-empty-p prefix))
                    (get-text-property 0 'face flycheck-mode-line-prefix)))
         (start (and face
                     (string-match (regexp-quote prefix) text))))
    (if start
        (list (substring text 0 start)
              flycheck-mode-line-prefix
              (substring text (+ start (length prefix))))
      text)))

(defun cat/flycheck-doom-modeline-prefix (segment)
  "Prepend the Flycheck prefix to Doom Modeline SEGMENT."
  (if (or (not (stringp segment)) (string-empty-p segment))
      segment
    (let ((prefix (copy-sequence flycheck-mode-line-prefix)))
      (dolist (property '(help-echo mouse-face local-map))
        (when-let* ((value (get-text-property 0 property segment)))
          (put-text-property 0 (length prefix) property value prefix)))
      (setq doom-modeline--flycheck
            (concat prefix (doom-modeline-vspc) segment)))))

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-mode-line '(:eval (cat/flycheck-mode-line-status)))
  (flycheck-mode-line-prefix (+with-icon "nf-oct-checklist"))
  (flycheck-indication-mode 'left-margin)
  (flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list-or-sideline))

(use-package flycheck-pos-tip
  :cat pos-tip
  :hook (flycheck-mode . flycheck-pos-tip-mode))

(use-package flycheck-posframe
  :cat posframe
  :hook (flycheck-mode . flycheck-posframe-mode))

(with-eval-after-load 'doom-modeline
  (unless (advice-member-p #'cat/flycheck-doom-modeline-prefix
                           'doom-modeline-update-flycheck)
    (advice-add 'doom-modeline-update-flycheck :filter-return
                #'cat/flycheck-doom-modeline-prefix))
  (add-hook 'doom-modeline-mode-hook #'cat/flycheck-setup-doom-modeline))

(defun cat/flycheck-setup-doom-modeline ()
  "Toggle flycheck minor mode indicator"
  (if doom-modeline-mode
      (setq flycheck-mode-line nil)
    (setq flycheck-mode-line '(:eval (cat/flycheck-mode-line-status)))))
