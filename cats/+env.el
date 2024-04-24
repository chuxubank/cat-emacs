;; -*- lexical-binding: t; -*-

(use-package exec-path-from-shell
  :unless IS-WINDOWS
  :demand t
  :custom
  (exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-variables '("PATH" "MANPATH" "JAVA_HOME"))
  :config
  (exec-path-from-shell-initialize))
