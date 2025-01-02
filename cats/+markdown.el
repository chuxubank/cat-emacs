;; -*- lexical-binding: t; -*-

(defun markdown-xwidget-auto-theme ()
  "Adjust `markdown-xwidget-github-theme' and `markdown-xwidget-mermaid-theme' to align with Emacs' current theme."
  (setq-default markdown-xwidget-github-theme (if (+dark-mode-p) "dark" "light")
                markdown-xwidget-mermaid-theme (if (+dark-mode-p) "dark" "default")))

(use-package mustache)

(use-package markdown-xwidget
  :vc (markdown-xwidget
       :url "https://github.com/cfclrk/markdown-xwidget"
       :rev :newest)
  :demand t
  :after markdown-mode
  :bind
  (:map markdown-mode-command-map
        ("x" . markdown-xwidget-preview-mode))
  :config
  (add-hook 'cat-theme-refresh-hook #'markdown-xwidget-auto-theme)
  (markdown-xwidget-auto-theme))
