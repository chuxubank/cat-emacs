;; -*- lexical-binding: t; -*-

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
