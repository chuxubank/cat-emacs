;; -*- lexical-binding: t; -*-

(pretty-hydra-define cat-vibe
  (:color teal :title (+with-icon "nf-fa-wand_sparkles" "Vibe Coding"))
  ("" ()))

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
  (gptel-expert-commands t)
  (gptel-model 'moonshotai/kimi-k2:free)
  (gptel-default-mode 'org-mode)
  :pretty-hydra
  ((:color teal :title (+with-icon "nf-dev-emacs" "GPTel"))
   ("Send"
    (("g" #'gptel "gptel")
     ("s" #'gptel-send "send")
     ("r" #'gptel-rewrite "rewrite"))
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
  (setq gptel--gemini
        (gptel-make-gemini "Gemini" :key 'gptel-api-key :stream t)
        gptel--openrouter
        (gptel-make-openai "OpenRouter"
          :host "openrouter.ai"
          :endpoint "/api/v1/chat/completions"
          :stream t
          :key 'gptel-api-key
          :models '(alibaba/tongyi-deepresearch-30b-a3b:free
                    deepseek/deepseek-chat-v3.1:free
                    deepseek/deepseek-r1-0528-qwen3-8b:free
                    deepseek/deepseek-r1-0528:free
                    deepseek/deepseek-r1:free
                    google/gemini-2.0-flash-exp:free
                    google/gemma-3-27b-it:free
                    meituan/longcat-flash-chat:free
                    meta-llama/llama-3.3-70b-instruct:free
                    microsoft/mai-ds-r1:free
                    moonshotai/kimi-k2:free
                    moonshotai/kimi-dev-72b:free
                    nvidia/nemotron-nano-9b-v2:free
                    openai/gpt-oss-20b:free
                    qwen/qwen3-235b-a22b:free
                    qwen/qwen3-30b-a3b:free
                    qwen/qwen3-coder:free
                    tngtech/deepseek-r1t2-chimera:free
                    z-ai/glm-4.5-air:free)))
  (setq gptel-backend gptel--openrouter))

(use-package gptel-magit
  :hook (magit-mode . gptel-magit-install)
  :custom
  (gptel-magit-backend gptel--openrouter)
  (gptel-magit-model 'tngtech/deepseek-r1t2-chimera:free)
  (gptel-magit-commit-prompt (gptel-prompts-poet (expand-file-name "git-commit.yml.j2" cat-prompt-dir)))
  :config
  (advice-add 'gptel-magit--generate :around #'cat/gptel-magit--generate-without-reasoning))

(defun cat/gptel-magit--generate-without-reasoning (orig-fn callback)
  "Advice around ORIG-FN to set `gptel-include-reasoning' to nil with CALLBACK."
  (let ((gptel-include-reasoning nil)
        (gptel--request-params (if (eq gptel-magit-backend gptel--openrouter)
                                   (list :reasoning (list :exclude t
                                                          :effort "minimal"))
                                 nil)))
    (funcall orig-fn callback)))

(use-package gptel-prompts
  :vc (gptel-prompts
       :url "https://github.com/jwiegley/gptel-prompts"
       :rev :newest)
  :commands #'gptel-prompts-poet
  :custom
  (gptel-prompts-directory cat-prompt-dir)
  (gptel-prompts-template-variables
   `(("user_name" . ,(or (getenv "USER") (user-login-name)))
     ("emacs_version" . ,(format "Emacs %s" emacs-version))
     ("git_commit_summary_max_length" . ,git-commit-summary-max-length)))
  :config
  (gptel-prompts-update)
  (gptel-prompts-add-update-watchers))

(use-package templatel)

(use-package chatgpt-shell
  :custom
  (chatgpt-shell-root-path (concat cat-local-dir "shell-maker/"))
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
