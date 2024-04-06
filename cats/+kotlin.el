;; -*- lexical-binding: t; -*-

(use-package kotlin-mode
  :ensure-system-package
  (kotlin-language-server))

(use-package kotlin-ts-mode
  :when EMACS29+
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(kotlin-ts-mode "kotlin-language-server"))))

(use-package flycheck-kotlin
  :hook (flycheck-mode . flycheck-kotlin-setup))

(use-package java-imports
  :hook ((java-mode kotlin-mode) . java-imports-scan-file)
  :custom
  (java-imports-find-block-function 'java-imports-find-place-sorted-block))
