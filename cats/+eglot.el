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
          ;; kotlin-mode
          ;; kotlin-ts-mode
          )
         . eglot-ensure) ; See `eglot-server-programs'
  :custom
  (eglot-connect-timeout (* 30 60))
  :mode-hydra
  (prog-mode
   ("LSP"
    (("e" eglot)))))
