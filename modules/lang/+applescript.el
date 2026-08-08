;; -*- lexical-binding: t; -*-

(use-package applescript-mode
  :font-role code-apple)

(use-package ob-applescript
  :demand t
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(applescript . t)))
