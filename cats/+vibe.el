;; -*- lexical-binding: t; -*-

(pretty-hydra-define cat-vibe
  (:color teal :title (+with-icon "nf-fa-wand_sparkles" "Vibe Coding"))
  ("" ()))

(use-package gptel-model-updater
  :ensure nil
  :commands
  (gptel-model-updater-update-backend
   gptel-model-updater-update-all)
  :hook
  (cat-idle-preload-hook . gptel-model-updater-update-all)
  :pretty-hydra
  (cat-vibe
   ("GPTel"
    (("u" #'gptel-model-updater-update-backend "update")
     ("U" #'gptel-model-updater-update-all "update all"))))
  :config
  (setq gptel--backends
        '(gptel--gemini
          gptel--openrouter
          gptel--ollama
          gptel--iv)))

(use-package gptel
  :delight " 󱡄"
  :custom
  (gptel-expert-commands t)
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
   ("GPTel"
    (("g" #'gptel-hydra/body "gptel"))))
  :config
  (setq gptel--gemini
        (gptel-make-gemini "Gemini" :key 'gptel-api-key :stream t)
        gptel--openrouter
        (gptel-make-openai "OpenRouter"
          :host "openrouter.ai/api"
          :key 'gptel-api-key
          :stream t)
        gptel--iv
        (gptel-make-openai "IV"
          :host "llm.invalley.co"
          :protocol "http"
          :key 'gptel-api-key
          :stream t)
        gptel--ollama
        (gptel-make-ollama "Ollama"
          :host "localhost:11434"
          :stream t))
  (setq gptel-backend gptel--iv
        gptel-model 'claude-sonnet-4-6))

(use-package gptel-magit
  :hook (magit-mode . gptel-magit-install)
  :custom
  (gptel-magit-commit-prompt (gptel-prompts-poet (expand-file-name "git-commit.yml.j2" cat-prompt-dir)))
  :config
  (setq gptel-magit-backend gptel--iv
        gptel-magit-model 'deepseek-v3.2)
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
  (chatgpt-shell-model-version "gemma4")
  :pretty-hydra
  (cat-vibe
   ("Shell"
    (("c" #'chatgpt-shell-transient "chatgpt shell"))))
  :config
  (chatgpt-shell-ollama-load-models :override t))

;;; agent

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

(use-package agent-shell
  :delight
  (agent-shell-ui-mode "")
  (agent-shell-completion-mode "")
  :bind
  (:map agent-shell-ui-mode-map
        ("C-c C-p" . agent-shell-ui-backward-block)
        ("C-c C-n" . agent-shell-ui-forward-block))
  :custom
  (agent-shell-dot-subdir-function #'agent-shell--dot-subdir-in-cache)
  :pretty-hydra
  (agent-shell
   (:color teal :title (+with-icon "nf-dev-terminal" "Agent Shell"))
   ("Action"
    (("s" #'agent-shell "agent-shell")
     ("n" #'agent-shell-new-shell "new shell"))))
  (cat-vibe
   ("Shell"
    (("s" #'agent-shell/body "agent-shell"))))
  :config
  (defun agent-shell--dot-subdir-in-cache (subdir)
    "Return path to agent-shell/SUBDIR under the `cat-cache-dir'.

For example:

  (agent-shell--dot-subdir-in-cache \"screenshots\")
  => \"/path/to/cat-cache-dir/agent-shell/project-dir/screenshots\""
    (concat cat-cache-dir "agent-shell" (agent-shell-cwd) subdir)))

(use-package mcp-server
  :vc (:url "https://github.com/rhblind/emacs-mcp-server")
  :hook (cat-idle-preload-hook . mcp-server-start-unix)
  :custom
  (mcp-server-socket-directory cat-cache-dir))
