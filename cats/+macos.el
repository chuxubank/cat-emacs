;; -*- lexical-binding: t; -*-

(setq mac-system-move-file-to-trash-use-finder t)

(use-package ns-auto-titlebar
  :config
  (and (or (daemonp)
           (display-graphic-p))
       (ns-auto-titlebar-mode +1)))

(use-package osx-location
  :defer t
  :config
  (add-hook 'osx-location-changed-hook
            (lambda ()
              (setq calendar-latitude osx-location-latitude
                    calendar-longitude osx-location-longitude
                    calendar-location-name (format "%s, %s" osx-location-latitude osx-location-longitude)))))

(use-package mac-pseudo-daemon
  :disabled
  :when IS-MACPORT
  :config
  (mac-pseudo-daemon-mode))

(when IS-MACPORT
  ;; See Info node `(emacs) Mac Fullscreen' for more information.
  (menu-bar-mode 1))

(define-key global-map [(meta f11)] #'toggle-frame-fullscreen)
