;; -*- lexical-binding: t; -*-

(use-package makefile-executor
  :delight (makefile-executor-mode
            (:eval (+with-icon "nf-cod-run_all" " ")))
  :hook (makefile-mode . makefile-executor-mode))
