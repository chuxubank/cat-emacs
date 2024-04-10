;; -*- lexical-binding: t; -*-

(use-package kotlin-mode
  :ensure-system-package
  (kotlin-language-server))

(use-package kotlin-ts-mode
  :when EMACS29+
  :mode-hydra
  (("LSP"
    (("e" eglot-hydra/body "eglot"))
    "Test"
    (("t" kotlin-ts-mode-goto-test-file "go to test file")
     ("r" kotlin-ts-mode-run-current-test-function "run current test function")
     ("R" kotlin-ts-mode-run-current-test-class "run current test class")))))

(use-package flycheck-kotlin
  :hook (flycheck-mode . flycheck-kotlin-setup))

(use-package java-imports
  :hook ((java-mode kotlin-mode) . java-imports-scan-file)
  :custom
  (java-imports-find-block-function 'java-imports-find-place-sorted-block))
