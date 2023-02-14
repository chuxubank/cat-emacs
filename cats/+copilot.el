;; -*- lexical-binding: t; -*-

(use-package copilot
  :defer t
  :straight (copilot :host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :config
  (with-eval-after-load 'company
    ;; disable inline previews
    (delq 'company-preview-if-just-one-frontend company-frontends))

  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

(add-hook 'prog-mode-hook 'copilot-mode)
