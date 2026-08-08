;; -*- lexical-binding: t; -*-

(use-package kotlin-mode
  :font-role code-jvm
  :custom
  (kotlin-args-repl '("-Xrepl")))

(use-package kotlin-ts-mode
  :font-role code-jvm)

(use-package kotlin-ts-test
  :vc (:url "https://github.com/cat-emacs/kotlin-ts-test")
  :when EMACS29+
  :after kotlin-ts-mode
  :custom
  (kotlin-ts-test-task-alist '(("common" . "desktopTest")))
  :mode-transient
  (kotlin-ts-mode
   ["Test"
    ("t" "go to test file" kotlin-ts-test-goto-file)
    ("r" "run current test class" kotlin-ts-test-run-class)
    ("R" "run current test function" kotlin-ts-test-run-function)
    ("l" "rerun last test" kotlin-ts-test-rerun)]))

(use-package ob-kotlin
  :vc (:url "https://github.com/cat-emacs/ob-kotlin")
  :demand
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(kotlin . t)))

(use-package flycheck-kotlin
  :hook (flycheck-mode . flycheck-kotlin-setup))
