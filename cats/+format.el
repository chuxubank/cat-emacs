;; -*- lexical-binding: t; -*-

(use-package apheleia
  :ensure-system-package
  (prettier . "pnpm add -g prettier")
  :hook (after-init . apheleia-global-mode)
  :custom
  (apheleia-remote-algorithm 'local)
  (apheleia-mode-lighter " 󰿞")
  :config
  (setf (alist-get 'latexindent apheleia-formatters)
        '("latexindent" "-l" "--logfile=/dev/null")))
