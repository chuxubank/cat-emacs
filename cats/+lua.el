;; -*- lexical-binding: t; -*-

(use-package lua-mode
  :ensure-system-package
  (stylua . stylua)
  :custom
  (lua-indent-level 2))

(use-package lua-ts-mode
  :custom
  (lua-ts-indent-offset 2))
