;; -*- lexical-binding: t; -*-

(defun markdown-xwidget-dark-theme ()
  (setq markdown-xwidget-github-theme "dark"))

(defun markdown-xwidget-light-theme ()
  (setq markdown-xwidget-github-theme "light"))

(use-package markdown-xwidget
  :vc (markdown-xwidget
       :url "https://github.com/cfclrk/markdown-xwidget"
       :rev :newest)
  :demand t
  :after markdown-mode
  :custom
  (markdown-xwidget-github-theme (symbol-name (frame-parameter nil 'background-mode)))
  :bind
  (:map markdown-mode-command-map
        ("x" . markdown-xwidget-preview-mode)))
