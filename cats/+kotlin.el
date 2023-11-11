;; -*- lexical-binding: t; -*-

(use-package kotlin-mode
  :ensure-system-package
  (kotlin-language-server . kotlin-language-server)
  :defer t)

(use-package kotlin-ts-mode
  :when EMACS29+
  :defer t
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(kotlin-ts-mode "kotlin-language-server"))))

(use-package flycheck-kotlin
  :after flycheck (:any kotlin-mode kotlin-ts-mode)
  :config
  (flycheck-kotlin-setup))
