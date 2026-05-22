;; -*- lexical-binding: t; -*-

(use-package kotlin-mode
  :custom
  (kotlin-args-repl '("-Xrepl")))

(use-package kotlin-ts-mode)

(use-package kotlin-ts-test
  :ensure nil
  :when EMACS29+
  :after kotlin-ts-mode
  :custom
  (kotlin-ts-test-task-alist '(("common" . "desktopTest")))
  :mode-hydra
  (kotlin-ts-mode
   ("Test"
    (("t" kotlin-ts-test-goto-file "go to test file")
     ("r" kotlin-ts-test-run-class "run current test class")
     ("R" kotlin-ts-test-run-function "run current test function")
     ("l" kotlin-ts-test-rerun "rerun last test")))))

(use-package ob-kotlin
  :vc (:url "https://github.com/chuxubank/ob-kotlin")
  :demand
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(kotlin . t)))

(use-package flycheck-kotlin
  :hook (flycheck-mode . flycheck-kotlin-setup))
