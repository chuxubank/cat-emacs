(use-package pass
  :bind
  ("C-c p p" . #'pass))

(use-package pinentry
  :after pass
  :config
  (pinentry-start))
