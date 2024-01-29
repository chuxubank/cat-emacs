;; -*- lexical-binding: t; -*-

(use-package ns-auto-titlebar
  :when (or (daemonp)
            (display-graphic-p))
  :hook (after-init . ns-auto-titlebar-mode))

(use-package osx-location
  :config
  (add-hook 'osx-location-changed-hook
            (lambda ()
              (setq calendar-latitude osx-location-latitude
                    calendar-longitude osx-location-longitude
                    calendar-location-name (format "%s, %s" osx-location-latitude osx-location-longitude)))))

(use-package exec-path-from-shell
  :when (length< (getenv "PATH") 50)
  :config
  (exec-path-from-shell-initialize))

(when IS-MACPORT
  (when (display-graphic-p)
    (setq mac-system-move-file-to-trash-use-finder t))
  ;; See Info node `(emacs) Mac Fullscreen' for more information.
  (menu-bar-mode 1)

  ;; see https://github.com/railwaycat/homebrew-emacsmacport/issues/52
  (use-package mac-pseudo-daemon
    :hook (after-init . mac-pseudo-daemon-mode)
    :config
    (advice-add #'macpd-make-new-default-frame :after (lambda (_) (cat-client-frame-config))))
  (use-package server
    :hook (after-init . server-mode)))

(define-key global-map (kbd "C-s-f") #'toggle-frame-fullscreen)
