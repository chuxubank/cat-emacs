;; -*- lexical-binding: t; -*-

(use-package exec-path-from-shell
  :unless IS-WINDOWS
  :demand t
  :custom
  (exec-path-from-shell-arguments '("-l"))
  :config
  (exec-path-from-shell-initialize))
