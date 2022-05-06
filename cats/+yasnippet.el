;; -*- lexical-binding: t; -*-

(use-package yasnippet
  :defer t)

(with-eval-after-load 'yasnippet
  (yas-reload-all))

(add-hook 'prog-mode-hook 'yas-minor-mode)
