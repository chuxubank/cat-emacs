;; -*- lexical-binding: t; -*-

(use-package copilot
  :straight (copilot :host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :hook (prog-mode . copilot-mode)
  :bind
  (:map copilot-completion-map
        ("<tab>" . 'copilot-accept-completion)
        ("TAB" . 'copilot-accept-completion)
        ("C-TAB" . 'copilot-accept-completion-by-word)
        ("C-<tab>" . 'copilot-accept-completion-by-word))
  :config
  (with-eval-after-load 'company
    ;; disable inline previews
    (delq 'company-preview-if-just-one-frontend company-frontends)))
