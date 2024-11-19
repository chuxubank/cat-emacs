;; -*- lexical-binding: t; -*-

(use-package eglot
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
   ("Actions"
    (("s" #'eglot "start")
     ("S" #'eglot-reconnect "reconnect")
     ("k" #'eglot-shutdown "eglot shutdown")
     ("K" #'eglot-shutdown-all "eglot shutdown all")
     ("r" #'eglot-rename "rename")
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
     ("R" #'eglot-code-action-rewrite "rewrite")))))
