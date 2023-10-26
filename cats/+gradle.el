;; -*- lexical-binding: t; -*-

(use-package groovy-mode
  :mode "\\.gradle\\'")

(use-package groovy-imports
  :after groovy-mode
  :bind
  (:map groovy-mode-map
        ("M-I" . #'groovy-imports-add-import-dwim))
  :config
  (add-hook 'groovy-mode-hook 'groovy-imports-scan-file))

(use-package flymake-gradle
  :after flymake
  :config
  (flymake-gradle-add-hook))

(use-package flycheck-gradle
  :after flycheck
  :config
  (flycheck-gradle-setup))
