;; -*- lexical-binding: t; -*-

(pretty-hydra-define cat-agent
  (:color teal :title (+with-icon "nf-md-robot" "Agentic Coding"))
  ("" ()))

(use-package aidermacs
  :commands #'aidermacs-transient-menu
  :custom
  (aidermacs-backend 'vterm)
  (aidermacs-watch-files t)
  :pretty-hydra
  (cat-agent
   ("Aider"
    (("a" #'aidermacs-transient-menu "aidermacs")))))

(use-package aider
  :pretty-hydra
  (cat-agent
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
  (cat-agent
   ("Agent Shell"
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
