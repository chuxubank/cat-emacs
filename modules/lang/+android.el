;; -*- lexical-binding: t; -*-

(use-package android-mode
  :vc (android-mode :url "https://github.com/cat-emacs/emacs-studio"
                    :lisp-dir "android-mode/")
  :delight (android-mode (:eval (+with-icon "nf-md-android" " ")))
  :commands #'android-root
  :init
  (defun cat/android-mode ()
    (when (android-root) (android-mode t)))
  :hook ((find-file dired-mode) . cat/android-mode)
  :custom
  (android-mode-cache-dir (concat cat-cache-dir "android"))
  :minor-mode-transient
  (android-mode
   ["Start"
    ("a" "start app" android-start-app)
    ("r" "run" android-run)
    ("e" "emulator" android-start-emulator)]
   ["Build"
    ("c" "build" android-gradle-build)
    ("C" "clean" android-gradle-clean)
    ("t" "test" android-gradle-test)
    ("i" "install" android-gradle-install)
    ("u" "uninstall" android-gradle-uninstall)
    ("f" "flavors" android-print-flavor)
    ("R" "refresh" android-refresh-flavors)]))

(use-package compose-preview
  :vc (compose-preview :url "https://github.com/cat-emacs/emacs-studio"
                       :lisp-dir "compose-preview/")
  :minor-mode-transient
  (android-mode
   ["Compose"
    ("p" "preview" compose-preview-refresh)
    ("P" "open previews" compose-preview-open-results)
    ("s" "record snapshots" compose-preview-record)
    ("S" "verify snapshots" compose-preview-verify)
    ("v" "set variant" compose-preview-set-variant)]))

(use-package elogcat
  :vc (:url "https://github.com/cat-emacs/elogcat.el")
  :bind
  (:map elogcat-mode-map
        ("n" . #'next-line)
        ("p" . #'previous-line))
  :minor-mode-transient
  (android-mode
   ["Log"
    ("l" "elogcat" elogcat)])
  :config
  (add-hook 'elogcat-mode-hook #'meow-motion-mode)
  (add-hook 'elogcat-mode-hook #'cat/enable-doom-modeline-minor-modes))
