;; -*- lexical-binding: t; -*-

(use-package codeium
  :vc (:url "https://github.com/Exafunction/codeium.el" :rev :newest)
  :defer t
  :init
  (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
  :custom
  (codeium-command-executable (expand-file-name "language_server" cat-codeium-dir))
  (codeium/metadata/api_key (f-read-text cat-codeium-api-key-file))
  :config
  ;; get codeium status in the modeline
  (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
  (add-to-list 'global-mode-string '(:eval (car-safe codeium-mode-line)) t)
  ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
  (setq codeium-api-enabled
        (lambda (api)
          (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))

  (defun cat-disable-codeium ()
    (setq-local completion-at-point-functions (delq 'codeium-completion-at-point completion-at-point-functions)))

  (add-hook 'pass-view-mode-hook #'cat-disable-codeium))
