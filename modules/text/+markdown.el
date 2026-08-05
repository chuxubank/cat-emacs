;; -*- lexical-binding: t; -*-

(defun markdown-xwidget-auto-theme ()
  "Adjust `markdown-xwidget-github-theme' and `markdown-xwidget-mermaid-theme' to align with Emacs' current theme."
  (setq-default markdown-xwidget-github-theme (if (+dark-mode-p) "dark" "light")
                markdown-xwidget-mermaid-theme (if (+dark-mode-p) "dark" "default")))

(use-package mustache)

(use-package markdown-mode
  :cat-font (body
             :faces ((markdown-header-face heading)
                     (markdown-header-face-1 title)
                     (markdown-table-face table)
                     (markdown-code-face code)
                     (markdown-inline-code-face code)))
  :pin melpa-stable
  :mode ("README\\.md\\'" . gfm-mode)
  :custom
  (markdown-command "pandoc"))

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

(use-package grip-mode
  :delight (grip-mode (:eval (+with-icon "nf-cod-open_preview" " ")))
  :demand t
  :after markdown-mode
  :bind
  (:map markdown-mode-command-map
        ("g" . grip-mode))
  :custom
  (grip-command 'auto))
