;; -*- lexical-binding: t; -*-

(use-package apheleia
  :ensure-system-package
  (prettier . "bun add -g prettier")
  :hook (after-init . apheleia-global-mode)
  :custom
  (apheleia-remote-algorithm 'local)
  (apheleia-mode-lighter " 󰿞")
  (apheleia-formatters-respect-indent-level nil)
  :config
  (setf (alist-get 'latexindent apheleia-formatters)
        '("latexindent" "-l" "--logfile=/dev/null"))
  (+add-to-list-multi 'apheleia-inhibit-functions
                      #'cat-is-type-break-buffer
                      #'cat-is-node-package-lock-buffer))
