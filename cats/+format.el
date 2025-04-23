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
  (setf (alist-get 'dprint apheleia-formatters)
        `("dprint" "fmt" "--stdin" filepath "--config"
          ,(expand-file-name "dprint.json" (getenv "XDG_CONFIG_HOME"))))
  (add-to-list 'apheleia-mode-alist
               '(toml-ts-mode . dprint))
  (add-to-list 'apheleia-skip-functions
               #'cat-chezmoi-mode-p)
  (+add-to-list-multi 'apheleia-inhibit-functions
                      #'cat-type-break-buffer-p
                      #'cat-node-package-lock-buffer-p))
