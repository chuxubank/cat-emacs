;; -*- lexical-binding: t; -*-

(use-package mu4e
  
  :ensure nil
  :custom
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-use-fancy-chars t)
  (mu4e-sent-messages-behavior 'delete)
  (shr-color-visible-luminance-min 80))

(with-eval-after-load 'selectrum
  (setq mu4e-completing-read-function #'selectrum-completing-read))
