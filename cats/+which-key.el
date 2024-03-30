;; -*- lexical-binding: t; -*-

(use-package which-key
  :hook (after-init . which-key-mode)
  :custom
  (which-key-use-C-h-commands nil)
  (which-key-lighter nil)
  (which-key-dont-use-unicode t))
