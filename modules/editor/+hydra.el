;; -*- lexical-binding: t; -*-

(defun cat-hydra-posframe-update-font ()
  "Use the mono font role for Hydra posframes."
  (setq hydra-posframe-font
        (or (cat--first-existing-font 'mono)
            (car (cat--font-list 'mono)))))

(use-package hydra-posframe
  :vc (:url "https://github.com/Ladicle/hydra-posframe")
  :hook ((after-init . cat-hydra-posframe-update-font)
         (after-init . hydra-posframe-mode))
  :custom
  (hydra-posframe-poshandler #'posframe-poshandler-window-bottom-center)
  (hydra-posframe-font (car (cat--font-list 'mono)))
  (hydra-posframe-border-width 10))
