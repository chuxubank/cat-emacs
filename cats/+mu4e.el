;; -*- lexical-binding: t; -*-

(use-package mu4e
  :commands #'mu4e
  :ensure-system-package
  (mu)
  (mbsync . isync)
  :ensure nil
  :custom
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-update-interval 300)
  (mu4e-use-fancy-chars t)
  (mu4e-sent-messages-behavior 'delete)
  (mu4e-read-option-use-builtin nil)
  (mu4e-completing-read-function 'completing-read)
  :pretty-hydra
  ((:color teal :title (+with-icon "nf-oct-mail" "Mail"))
   ("Mu4e"
    (("m" #'mu4e "mu4e")
     ("k" #'mu4e-quit "quit")
     ("u" #'mu4e-update-index "update")
     ("o" #'mu4e-overview "overview")))))

(with-eval-after-load 'selectrum
  (setq mu4e-completing-read-function #'selectrum-completing-read))

(use-package mu4e-column-faces
  :demand
  :after mu4e
  :config (mu4e-column-faces-mode))

(use-package mu4e-overview)

(use-package mu4e-views
  :demand
  :after mu4e
  :bind
  (:map mu4e-headers-mode-map
	    ("v" . mu4e-views-mu4e-select-view-msg-method)
	    ("M-n" . mu4e-views-cursor-msg-view-window-down)
	    ("M-p" . mu4e-views-cursor-msg-view-window-up)
        ("f" . mu4e-views-toggle-auto-view-selected-message)
        ("i" . mu4e-views-mu4e-view-as-nonblocked-html))
  :custom
  (mu4e-views-default-view-method "html")
  (mu4e-views-next-previous-message-behaviour 'stick-to-current-window)
  (mu4e-views-auto-view-selected-message t)
  :config
  (mu4e-views-mu4e-use-view-msg-method "html"))
