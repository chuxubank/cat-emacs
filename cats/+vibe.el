;; -*- lexical-binding: t; -*-

(use-package codeium
  :pin jcs-elpa
  :bind
  (:map cat-cape-map
        ("c" . cat-cape-codeium))
  :init
  (defun cat-cape-codeium (&optional interactive)
    "Allow codeium capf to be run by itself"
    (interactive (list t))
    (when interactive
      ;; if also testing copilot, clear their overlay before showing capf popup
      (when (bound-and-true-p copilot-mode) (copilot-clear-overlay))
      (cape-interactive #'codeium-completion-at-point)))
  :custom
  (codeium-command-executable (expand-file-name "language_server" cat-codeium-dir))
  :config
  ;; get codeium status in the modeline
  (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
  (add-to-list 'global-mode-string '(:eval (car-safe codeium-mode-line)) t)
  ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
  (setq codeium-api-enabled
        (lambda (api)
          (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion)))))

(use-package aidermacs
  :commands #'aidermacs-transient-menu
  :custom
  (aidermacs-backend 'vterm))
