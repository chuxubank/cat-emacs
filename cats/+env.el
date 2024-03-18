;; -*- lexical-binding: t; -*-

(use-package exec-path-from-shell
  :demand t
  :when (display-graphic-p)
  :config
  (exec-path-from-shell-initialize))
