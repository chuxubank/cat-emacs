;; -*- lexical-binding: t; -*-

(use-package applescript-mode)

(use-package ob-applescript
  :demand t
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(applescript . t)))
