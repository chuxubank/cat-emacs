;; -*- lexical-binding: t; -*-

(use-package plantuml-mode
  :defer t
  :custom
  (plantuml-default-exec-mode 'executable))

(use-package flycheck-plantuml
  :after plantuml-mode
  :config
  (flycheck-plantuml-setup))

(with-eval-after-load 'ob-plantuml
  (setq org-plantuml-exec-mode 'plantuml)
  (add-to-list 'org-babel-default-header-args:plantuml
               '(:cmdline . "-charset utf-8")))
