;; -*- lexical-binding: t; -*-

(use-package emms
  :custom
  (emms-player-list '(emms-player-mpv))
  :config
  (require 'emms-setup)
  (emms-all))
