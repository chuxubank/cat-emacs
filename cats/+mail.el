;; -*- lexical-binding: t; -*-

(pretty-hydra-define cat-mail
  (:color teal :title (+with-icon "nf-oct-mail" "Mail"))
  ("Gnus"
   (("g" #'gnus "Gnus"))))

(use-package mu4e
  :commands #'mu4e
  :ensure-system-package
  (mu)
  (mbsync . isync)
  :ensure nil
  :bind
  (:map mu4e-main-mode-map
        ("q" . #'bury-buffer)
        ("Q" . #'mu4e-quit)
        ("u" . #'mu4e-update-index))
  :custom
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-update-interval 300)
  (mu4e-use-fancy-chars t)
  (mu4e-sent-messages-behavior 'delete)
  (mu4e-read-option-use-builtin nil)
  (mu4e-completing-read-function 'completing-read)
  :pretty-hydra
  (cat-mail
   ("Mu4e"
    (("m" #'mu4e "mu4e")
     ("k" #'mu4e-quit "quit")
     ("u" #'mu4e-update-index "update")))))

(use-package mu4e-column-faces
  :demand
  :after mu4e
  :config (mu4e-column-faces-mode))

(use-package mu4e-overview
  :pretty-hydra
  (cat-mail
   ("Mu4e"
    (("o" #'mu4e-overview "overview")))))

(use-package gnus
  :ensure nil
  :custom
  (gnus-select-method '(nntp "news.gmane.io")))
