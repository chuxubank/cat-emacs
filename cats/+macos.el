;; -*- lexical-binding: t; -*-

(setq mac-system-move-file-to-trash-use-finder t)

(use-package ns-auto-titlebar
  :when (or (daemonp)
            (display-graphic-p))
  :hook after-init)

(use-package osx-location
  :defer t
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
  ;; See Info node `(emacs) Mac Fullscreen' for more information.
  (menu-bar-mode 1))

(define-key global-map (kbd "C-s-f") #'toggle-frame-fullscreen)
