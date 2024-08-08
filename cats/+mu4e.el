;; -*- lexical-binding: t; -*-

(use-package mu4e
  :commands #'mu4e
  :ensure-system-package
  (mu)
  (mbsync . isync)
  :ensure nil
  :custom
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-use-fancy-chars t)
  (mu4e-sent-messages-behavior 'delete)
  (mu4e-read-option-use-builtin nil)
  (mu4e-completing-read-function 'completing-read))

(with-eval-after-load 'selectrum
  (setq mu4e-completing-read-function #'selectrum-completing-read))
