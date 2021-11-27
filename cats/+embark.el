;; -*- lexical-binding: t; -*-

(use-package embark
  :bind
  ("C->" . embark-act)
  ("C-M->" . embark-dwim)
  ("C-h B" . embark-bindings)
  :init
  (setq prefix-help-command #'embark-prefix-help-command))
