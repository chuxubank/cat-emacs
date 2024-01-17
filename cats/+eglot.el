;; -*- lexical-binding: t; -*-

(use-package eglot
  :hook ((cmake-mode
          cmake-ts-mode
          c-mode
          c-ts-mode
          c++-mode
          c++-ts-mode
          objc-mode
          ;; kotlin-mode
          ;; kotlin-ts-mode
          )
         . eglot-ensure) ; See `eglot-server-programs'
  :custom
  (eglot-connect-timeout (* 30 60)))

(use-package flycheck-eglot
  :hook (flycheck-mode . global-flycheck-eglot-mode))
