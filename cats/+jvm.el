;; -*- lexical-binding: t; -*-

(use-package kotlin-mode)

(use-package kotlin-ts-mode
  :when EMACS29+
  :mode-hydra
  (("Test"
    (("t" kotlin-ts-mode-goto-test-file "go to test file")
     ("r" kotlin-ts-mode-run-current-test-function "run current test function")
     ("R" kotlin-ts-mode-run-current-test-class "run current test class")))))

(use-package flycheck-kotlin
  :hook (flycheck-mode . flycheck-kotlin-setup))

(use-package java-imports
  :disabled
  :hook ((java-mode kotlin-mode) . java-imports-scan-file)
  :custom
  (java-imports-find-block-function 'java-imports-find-place-sorted-block))

(use-package eglot-java
  :disabled
  :hook ((java-mode
          java-ts-mode) . eglot-java-mode)
  :custom
  (eglot-java-eglot-server-programs-manual-updates t)
  (eglot-java-eclipse-jdt-cache-directory (concat cat-cache-dir "jdt/"))
  (eglot-java-junit-platform-console-standalone-jar (concat cat-etc-dir "junit-platform-console-standalone.jar"))
  ;; Prevent auto install java lsp server
  (eglot-java-server-install-dir cat-etc-dir))

(use-package ob-kotlin
  :vc (ob-kotlin
       :url "https://github.com/chuxubank/ob-kotlin"
       :rev patch-1)
  :demand
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(kotlin . t)))
