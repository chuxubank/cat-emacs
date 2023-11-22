;; -*- lexical-binding: t; -*-

(use-package wanderlust
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
