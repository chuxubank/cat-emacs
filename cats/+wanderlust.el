;; -*- lexical-binding: t; -*-

(use-package wanderlust
  :defer t
  :config
  (when EMACS28+
    (setq elmo-passwd-storage-type 'auth-source))
  (if (boundp 'mail-user-agent)
      (setq mail-user-agent 'wl-user-agent))
  (if (fboundp 'define-mail-user-agent)
      (define-mail-user-agent
        'wl-user-agent
        'wl-user-agent-compose
        'wl-draft-send
        'wl-draft-kill
        'mail-send-hook)))

(when (featurep 'meow)
  (add-hook 'wl-folder-mode-hook #'cat-manual-motion-mode)
  (add-hook 'mime-view-mode-hook #'cat-manual-motion-mode))
