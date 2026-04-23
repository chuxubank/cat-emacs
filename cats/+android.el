;; -*- lexical-binding: t; -*-

(pretty-hydra-define cat-android
  (:color teal :title (+with-icon "nf-md-android" "Android"))
  ("" ()))

(use-package android-mode
  :ensure nil
  :delight " 󰀲"
  :commands #'android-root
  :init
  (defun +android-mode ()
    (when (android-root) (android-mode t)))
  :hook ((find-file dired-mode) . +android-mode)
  :pretty-hydra
  (cat-android
   ("Start"
    (("a" #'android-start-app "start app")
     ("r" #'android-run "run")
     ("e" #'android-start-emulator "emulator"))
    "Build"
    (("c" #'android-gradle-build "build")
     ("C" #'android-gradle-clean "clean")
     ("t" #'android-gradle-test "test")
     ("i" #'android-gradle-install "install")
     ("u" #'android-gradle-uninstall "uninstall")
     ("f" #'android-print-flavor "flavors")
     ("R" #'android-refresh-flavors "refresh")))))

(use-package elogcat
  :vc (:url "https://github.com/chuxubank/elogcat.el")
  :bind
  (:map elogcat-mode-map
        ("n" . #'next-line)
        ("p" . #'previous-line))
  :pretty-hydra
  (cat-android
   ("Log"
    (("l" #'elogcat "elogcat"))))
  :config
  (add-hook 'elogcat-mode-hook #'meow-motion-mode)
  (add-hook 'elogcat-mode-hook #'cat-enable-doom-modeline-minor-modes))
