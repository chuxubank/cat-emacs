;; -*- lexical-binding: t; -*-

(pretty-hydra-define cat-vibe
  (:color teal :title (+with-icon "nf-fa-wand_sparkles" "Vibe Coding"))
  ("" ()))

(defcustom cat-codeium-dir
  (or (getenv "CODEIUM_DIR") "~/.codeium/")
  "Filename of the codeium folder."
  :type 'directory
  :group 'cat-emacs)

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
  (aidermacs-backend 'vterm)
  (aidermacs-watch-files t)
  :pretty-hydra
  (cat-vibe
   ("Aider"
    (("a" #'aidermacs-transient-menu "aidermacs")))))

(use-package aider
  :pretty-hydra
  (cat-vibe
   ("Aider"
    (("A" #'aider-transient-menu "aider.el"))))
  :config
  (aider-magit-setup-transients))

(use-package ob-aider
  :demand t
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(aider . t)))

(use-package gptel
  :delight " 󱡄"
  :custom
  (gptel-model 'gemini-2.5-pro-preview-06-05)
  (gptel-default-mode 'org-mode)
  :pretty-hydra
  ((:color teal :title (+with-icon "nf-dev-emacs" "GPTel"))
   ("Send"
    (("g" #'gptel "gptel")
     ("s" #'gptel-send "send"))
    "Tweak"
    (("m" #'gptel-menu "menu"))
    "Context"
    (("a" #'gptel-add "add/remove")
     ("A" #'gptel-add-file "add file"))
    "Org"
    (("t" #'gptel-org-set-topic "set topic")
     ("p" #'gptel-org-set-properties "set properties"))))
  (cat-vibe
   ("Fundamental"
    (("g" #'gptel-hydra/body "gptel"))))
  :config
  (setq gptel-backend (gptel-make-gemini "Gemini" :key 'gptel-api-key :stream t)))

(use-package gptel-magit
  :hook (magit-mode . gptel-magit-install)
  :custom
  (gptel-magit-model 'gemini-2.0-flash)
  :config
  (advice-add 'gptel-magit--generate :around #'cat/gptel-magit--generate-without-reasoning))

(defun cat/gptel-magit--generate-without-reasoning (orig-fn callback)
  "Advice around ORIG-FN to set `gptel-include-reasoning' to nil with CALLBACK."
  (let ((gptel-include-reasoning nil))
    (funcall orig-fn callback)))

(use-package gptel-prompts
  :vc (gptel-prompts
       :url "https://github.com/jwiegley/gptel-prompts"
       :rev :newest)
  :demand t
  :after gptel
  :custom
  (gptel-prompts-directory cat-prompt-dir)
  :config
  (gptel-prompts-update)
  (gptel-prompts-add-update-watchers))

(use-package chatgpt-shell
  :custom
  (chatgpt-shell-google-key
   (let ((found (car (auth-source-search
                      :host "generativelanguage.googleapis.com"
                      :user "apikey"
                      :require '(:user :secret)))))
     (if found
         (plist-get found :secret)
       (error "No matching entry found"))))
  (chatgpt-shell-model-version "gemini-2.5-flash-preview-05-20")
  :pretty-hydra
  ((:color teal :title (+with-icon "nf-fa-terminal" "ChatGPT Shell"))
   ("Shell"
    (("p" #'chatgpt-shell-prompt "prompt")
     ("P" #'chatgpt-shell-prompt-compose "prompt compose")
     ("y" #'chatgpt-shell-prompt-appending-kill-ring "yank"))
    "Code"
    (("d" #'chatgpt-shell-describe-code "describe code")
     ("r" #'chatgpt-shell-refactor-code "refactor code")
     ("c" #'chatgpt-shell-write-git-commit "write git commit")
     ("t" #'chatgpt-shell-generate-unit-test "generate unit test")
     ("f" #'chatgpt-shell-fix-error-at-point "fix error at point"))
    "Utils"
    (("e" #'chatgpt-shell-proofread-paragraph-or-region "proofread")
     ("i" #'chatgpt-shell-describe-image "describe image")
     ("q" #'chatgpt-shell-quick-insert "quick insert"))
    "Eshell"
    (("w" #'chatgpt-shell-eshell-whats-wrong-with-last-command "what's wrong")
     ("s" #'chatgpt-shell-eshell-summarize-last-command-output "summarize output"))))
  (cat-vibe
   ("Shell"
    (("c" #'chatgpt-shell-hydra/body "chatgpt shell")))))
