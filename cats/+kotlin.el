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

(use-package java-imports
  :hook ((java-mode kotlin-mode) . java-imports-scan-file)
  :custom
  (java-imports-find-block-function 'java-imports-find-place-sorted-block))
