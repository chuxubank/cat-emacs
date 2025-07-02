;; -*- lexical-binding: t; -*-

(use-package eglot
  :ignore-builtin
  :pin gnu
  :hook ((cmake-mode
          cmake-ts-mode
          c-mode
          c-ts-mode
          c++-mode
          c++-ts-mode
          objc-mode
          python-base-mode
          js-base-mode
          ;; kotlin-mode
          ;; kotlin-ts-mode
          )
         . eglot-ensure) ; See `eglot-server-programs'
  :custom
  (eglot-connect-timeout (* 30 60))
  :pretty-hydra
  ((:color teal :title (+with-icon "nf-cod-server_environment" "Eglot"))
   ("Commands"
    (("s" #'eglot "start")
     ("S" #'eglot-reconnect "reconnect")
     ("k" #'eglot-shutdown "shutdown")
     ("K" #'eglot-shutdown-all "shutdown all")
     ("l" #'eglot-events-buffer "event buffer"))
    "Actions"
    (("r" #'eglot-rename "rename")
     ("f" #'eglot-format "format")
     ("F" #'eglot-format-buffer "format buffer")
     ("d" #'eglot-find-declaration "declaration")
     ("i" #'eglot-find-implementation "implementations"))
    "Code Actions"
    (("c" #'eglot-code-actions "actions")
     ("o" #'eglot-code-action-organize-imports "organize imports")
     ("q" #'eglot-code-action-quickfix "quickfix")
     ("E" #'eglot-code-action-extract "extract")
     ("I" #'eglot-code-action-inline "inline")
     ("R" #'eglot-code-action-rewrite "rewrite"))))
  :mode-hydra
  ((prog-mode
    python-base-mode
    kotlin-ts-mode
    beancount-mode)
   ("LSP"
    (("e" eglot-hydra/body "eglot"))))
  :config
  (setf (alist-get '(kotlin-mode kotlin-ts-mode) eglot-server-programs nil nil #'equal)
        '("kotlin-lsp" "--stdio")))

(use-package lsp-proxy
  :delight " 󰑣"
  :commands (lsp-proxy-mode
             lsp-proxy-open-config-file)
  :ensure nil
  :pretty-hydra
  ((:color teal :title (+with-icon "nf-md-rocket" "LSP Proxy"))
   ("Commands"
    (("t" #'lsp-proxy-mode "toggle")
     ("s" #'lsp-proxy-workspace-restart "restart")
     ("S" #'lsp-proxy-restart "restart all")
     ("l" #'lsp-proxy-open-log-file "open log")
     ("c" #'lsp-proxy-open-config-file "open config"))
    "Actions"
    (("r" #'lsp-proxy-rename "rename")
     ("f" #'lsp-proxy-format-buffer "format")
     ("d" #'lsp-proxy-find-declaration "declaration")
     ("D" #'lsp-proxy-find-definition "definition")
     ("T" #'lsp-proxy-find-type-definition "type definition")
     ("i" #'lsp-proxy-find-implementations "implementations")
     ("." #'lsp-proxy-describe-thing-at-point "describe"))
    "Code Actions"
    (("a" #'lsp-proxy-execute-code-action "actions")
     ("e" #'lsp-proxy-execute-command "execute command")
     ("s" #'lsp-proxy-show-project-diagnostics "show diagnostics"))))
  :mode-hydra
  ((prog-mode
    python-base-mode
    kotlin-ts-mode
    beancount-mode)
   ("LSP"
    (("p" lsp-proxy-hydra/body "lsp proxy")))))
