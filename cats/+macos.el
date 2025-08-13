;; -*- lexical-binding: t; -*-

(use-package ns-auto-titlebar
  :when (or (daemonp)
            (display-graphic-p))
  :hook (after-init . ns-auto-titlebar-mode))

(defun cat/set-calendar-geo-from-osx-location ()
  (setq calendar-latitude osx-location-latitude
        calendar-longitude osx-location-longitude))

(use-package osx-location
  :hook
  (osx-location-changed . cat/set-calendar-geo-from-osx-location))

(when IS-MACPORT
  (when (display-graphic-p)
    (setq mac-system-move-file-to-trash-use-finder t))
  ;; See Info node `(emacs) Mac Fullscreen' for more information.
  (menu-bar-mode 1)

  ;; see https://github.com/railwaycat/homebrew-emacsmacport/issues/52
  (use-package mac-pseudo-daemon
    :when (display-graphic-p)
    :hook (after-init . mac-pseudo-daemon-mode)
    :config
    (advice-add #'macpd-make-new-default-frame
                :filter-return
                (lambda (frame)
                  (run-hook-with-args 'cat-setup-fonts-hook nil frame)
                  frame))))

(define-key global-map (kbd "C-s-f") #'toggle-frame-fullscreen)
(define-key global-map (kbd "s-q") #'save-buffers-kill-emacs)
