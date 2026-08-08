;; -*- lexical-binding: t; -*-

(use-package eglot
  :ignore-builtin
  :pin gnu
  :hook ((
          cmake-mode
          cmake-ts-mode
          c-mode
          c-ts-mode
          c++-mode
          c++-ts-mode
          objc-mode
          python-base-mode
          js-base-mode
          )
         . eglot-ensure) ; See `eglot-server-programs'
  :custom
  (eglot-connect-timeout (* 30 60))
  :transient
  (cat-eglot
   (:description (+with-icon "nf-cod-server_environment" nil " Eglot"))
   ["Commands"
    ("s" "start" eglot)
    ("S" "reconnect" eglot-reconnect)
    ("k" "shutdown" eglot-shutdown)
    ("K" "shutdown all" eglot-shutdown-all)
    ("l" "event buffer" eglot-events-buffer)]
   ["Actions"
    ("r" "rename" eglot-rename)
    ("f" "format" eglot-format)
    ("F" "format buffer" eglot-format-buffer)
    ("d" "declaration" eglot-find-declaration)
    ("i" "implementations" eglot-find-implementation)]
   ["Code Actions"
    ("c" "actions" eglot-code-actions)
    ("o" "organize imports" eglot-code-action-organize-imports)
    ("q" "quickfix" eglot-code-action-quickfix)
    ("E" "extract" eglot-code-action-extract)
    ("I" "inline" eglot-code-action-inline)
    ("R" "rewrite" eglot-code-action-rewrite)])
  :major-transient
  ((prog-mode
    beancount-mode
    kotlin-ts-mode
    python-base-mode
    yaml-ts-mode)
   ["LSP"
    ("e" "eglot" cat-eglot)])
  :config
  (setf (alist-get '(kotlin-mode kotlin-ts-mode) eglot-server-programs nil nil #'equal)
        '("kotlin-lsp" "--stdio")))

(use-package lsp-proxy
  :vc (:url "https://github.com/jadestrong/lsp-proxy")
  :delight (lsp-proxy-mode (:eval (+with-icon "nf-md-rocket" " ")))
  :custom
  (lsp-proxy-user-languages-config (cat-config-file "lsp-proxy/languages.toml"))
  :hook
  ((
    beancount-mode
    )
   . lsp-proxy-mode)
  :commands
  (lsp-proxy-mode
   lsp-proxy-open-config-file)
  :transient
  (cat-lsp-proxy
   (:description (+with-icon "nf-md-rocket" nil " LSP Proxy"))
   ["Commands"
    ("t" "toggle" lsp-proxy-mode)
    ("s" "restart" lsp-proxy-workspace-restart)
    ("S" "restart all" lsp-proxy-restart)
    ("l" "open log" lsp-proxy-open-log-file)
    ("c" "open config" lsp-proxy-open-config-file)]
   ["Actions"
    ("r" "rename" lsp-proxy-rename)
    ("f" "format" lsp-proxy-format-buffer)
    ("d" "declaration" lsp-proxy-find-declaration)
    ("D" "definition" lsp-proxy-find-definition)
    ("T" "type definition" lsp-proxy-find-type-definition)
    ("i" "implementations" lsp-proxy-find-implementations)
    ("." "describe" lsp-proxy-describe-thing-at-point)]
   ["Code Actions"
    ("a" "actions" lsp-proxy-execute-code-action)
    ("e" "execute command" lsp-proxy-execute-command)
    ("p" "show diagnostics" lsp-proxy-show-project-diagnostics)])
  :major-transient
  ((prog-mode
    beancount-mode
    kotlin-ts-mode
    python-base-mode
    yaml-ts-mode)
   ["LSP"
    ("p" "lsp proxy" cat-lsp-proxy)]))
