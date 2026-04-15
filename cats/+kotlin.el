;; -*- lexical-binding: t; -*-

(use-package kotlin-mode
  :unless EMACS29+
  :custom
  (kotlin-args-repl '("-Xrepl")))

(use-package kotlin-ts-mode
  :when EMACS29+
  :mode-hydra
  (("Test"
    (("t" kotlin-ts-mode-goto-test-file "go to test file")
     ("r" kotlin-ts-mode-run-current-test-function "run current test function")
     ("R" kotlin-ts-mode-run-current-test-class "run current test class")))))

(use-package ob-kotlin
  :vc (:url "https://github.com/chuxubank/ob-kotlin")
  :demand
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(kotlin . t)))

(use-package flycheck-kotlin
  :hook (flycheck-mode . flycheck-kotlin-setup))
