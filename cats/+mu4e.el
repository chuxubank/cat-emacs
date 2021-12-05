;; -*- lexical-binding: t; -*-

(use-package mu4e
  :commands #'mu4e
  :defer t
  :ensure nil
  :custom
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-use-fancy-chars t)
  :config
  (set-face-font 'mu4e-header-face cat-mono-font)
  (set-face-font 'mu4e-header-highlight-face cat-mono-font))

(when (featurep 'selectrum)
  (setq mu4e-completing-read-function #'selectrum-completing-read))

(define-key global-map (kbd "C-c u") #'mu4e)
