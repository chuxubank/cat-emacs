;; -*- lexical-binding: t; -*-

(pretty-hydra-define cat-vibe
  (:color teal :title (+with-icon "nf-fa-wand_sparkles" "Vibe Coding"))
  ("Aider"
   (("a" #'aidermacs-transient-menu "aidermacs"))))

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

(use-package aider
  :pretty-hydra
  (cat-vibe
   ("Aider"
    (("A" #'aider-transient-menu "aider.el")))))

(use-package ob-aider
  :demand t
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(aider . t)))

(use-package gptel
  :custom
  (gptel-model 'gemini-2.5-flash-preview-05-20)
  (gptel-default-mode 'org-mode)
  :pretty-hydra
  (cat-vibe
   ("gptel"
    (("g" #'gptel-send "gptel send")
     ("G" #'gptel "gptel"))))
  :config
  (setq gptel-backend (gptel-make-gemini "Gemini" :key 'gptel-api-key :stream t)))
