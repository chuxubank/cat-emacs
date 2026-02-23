;; -*- lexical-binding: t; -*-

(pretty-hydra-define cat-vibe
  (:color teal :title (+with-icon "nf-fa-wand_sparkles" "Vibe Coding"))
  ("" ()))

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
          :models '(openrouter/free
                    alibaba/tongyi-deepresearch-30b-a3b:free
                    arcee-ai/trinity-large-preview:free
                    arcee-ai/trinity-mini:free
                    deepseek/deepseek-chat-v3.1:free
                    deepseek/deepseek-r1-0528-qwen3-8b:free
                    deepseek/deepseek-r1-0528:free
                    deepseek/deepseek-r1:free
                    google/gemini-2.0-flash-exp:free
                    google/gemma-3-27b-it:free
                    kwaipilot/kat-coder-pro:free
                    meituan/longcat-flash-chat:free
                    meta-llama/llama-3.3-70b-instruct:free
                    nvidia/nemotron-3-nano-30b-a3b:free
                    nvidia/nemotron-nano-12b-v2-vl:free
                    nvidia/nemotron-nano-9b-v2:free
                    openai/gpt-oss-120b:free
                    openai/gpt-oss-20b:free
                    qwen/qwen3-4b:free
                    qwen/qwen3-coder:free
                    qwen/qwen3-next-80b-a3b-instruct:free
                    stepfun/step-3.5-flash:free
                    tngtech/deepseek-r1t2-chimera:free
                    tngtech/tng-r1t-chimera:free
                    z-ai/glm-4.5-air:free)))
  (setq gptel-backend gptel--openrouter))

(use-package gptel-magit
  :hook (magit-mode . gptel-magit-install)
  :custom
  (gptel-magit-backend gptel--openrouter)
  (gptel-magit-model 'openai/gpt-oss-120b:free)
  (gptel-magit-commit-prompt (gptel-prompts-poet (expand-file-name "git-commit.yml.j2" cat-prompt-dir)))
  :config
  (defun gptel-magit--generate (callback)
    "Generate a commit message for current magit repo.
Invokes CALLBACK with the generated message when done."
    (let ((gptel-include-reasoning nil)
          (gptel--request-params (if (eq gptel-magit-backend gptel--openrouter)
                                     (list :reasoning (list :exclude t
                                                            :effort "minimal"))
                                   nil))
          (diff (magit-git-output "diff" "--cached")))
      (gptel-magit--request diff
        :system gptel-magit-commit-prompt
        :context nil
        :callback (lambda (response info)
                    (print info)
                    (when (and (stringp response)
                               (not (string-empty-p response)))
                      (funcall callback response)))))))

(use-package gptel-prompts
  :vc (:url "https://github.com/jwiegley/gptel-prompts")
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
