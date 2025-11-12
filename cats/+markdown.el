;; -*- lexical-binding: t; -*-

(defun markdown-xwidget-auto-theme ()
  "Adjust `markdown-xwidget-github-theme' and `markdown-xwidget-mermaid-theme' to align with Emacs' current theme."
  (setq-default markdown-xwidget-github-theme (if (+dark-mode-p) "dark" "light")
                markdown-xwidget-mermaid-theme (if (+dark-mode-p) "dark" "default")))

(use-package mustache)

(use-package markdown-mode
  :pin melpa-stable)

(use-package md-babel
  :vc (:url "https://github.com/md-babel/md-babel.el")
  :demand t
  :after markdown-mode
  :bind
  (:map markdown-mode-command-map
        ("C-c" . md-babel-execute-block-at-point))
  :config
  (setq md-babel-path (executable-find "md-babel")))

(use-package markdown-xwidget
  :vc (:url "https://github.com/cfclrk/markdown-xwidget")
  :demand t
  :after markdown-mode
  :bind
  (:map markdown-mode-command-map
        ("x" . markdown-xwidget-preview-mode))
  :config
  (add-hook 'cat-theme-refresh-hook #'markdown-xwidget-auto-theme)
  (markdown-xwidget-auto-theme))
