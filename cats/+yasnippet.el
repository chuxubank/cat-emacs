;; -*- lexical-binding: t; -*-

(use-package yasnippet
  :delight (yas-minor-mode  nil)
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))
