(use-package gradle-mode
  :hook (kotlin-mode java-mode)
  :config
  (setq gradle-use-gradlew t
	gradle-gradlew-executable "./gradlew"))

(use-package groovy-mode
  :mode "\\.gradle\\'")

(use-package groovy-imports
  :after groovy-mode
  :bind
  (:map groovy-mode-map
	("M-I" . #'groovy-imports-add-import-dwim))
  :init
  (setq pcache-directory (concat cat-cache-dir "pcache/"))
  :config
  (add-hook 'groovy-mode-hook 'groovy-imports-scan-file))
