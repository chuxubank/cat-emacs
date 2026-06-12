;; -*- lexical-binding: t; -*-

(use-package poly-ansible
  :delight " 󱂚")

(use-package ansible
  :delight)

(use-package ansible-doc
  :delight)

(use-package flymake-ansible-lint
  :hook
  (ansible-mode . flymake-ansible-lint-setup)
  (ansible-mode . flymake-mode))

