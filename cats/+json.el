;; -*- lexical-binding: t; -*-

(setq js-indent-level 2)

(use-package json-mode
  :defer t)

(with-eval-after-load 'json-mode
  (define-key json-mode-map (kbd "C-c C-w") 'json-mode-kill-path))
