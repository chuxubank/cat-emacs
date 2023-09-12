;; -*- lexical-binding: t; -*-

(use-package codeium
  :straight (codeium :type git :host github :repo "Exafunction/codeium.el")
  :defer t
  :init
  (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
  :custom
  (codeium-directory (concat cat-etc-dir "codeium/"))
  (codeium-command-executable (concat codeium-directory "codeium_language_server"))
  :config
  (setq use-dialog-box nil) ;; do not use popup boxes
  ;; get codeium status in the modeline
  (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
  (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
  ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
  (setq codeium-api-enabled
        (lambda (api)
          (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
  (defun cat-codeium/document/text ()
    (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
  ;; if you change the text, you should also change the cursor_offset
  ;; warning: this is measured by UTF-8 encoded bytes
  (defun cat-codeium/document/cursor_offset ()
    (codeium-utf8-byte-length
     (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
  (setq codeium/document/text 'cat-codeium/document/text)
  (setq codeium/document/cursor_offset 'cat-codeium/document/cursor_offset))
