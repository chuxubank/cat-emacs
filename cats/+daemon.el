;; -*- lexical-binding: t; -*-

(defvar cat-font-load nil
  "Whether cat font has been loaded.")

;; see https://github.com/railwaycat/homebrew-emacsmacport/issues/52
(use-package mac-pseudo-daemon
  :when IS-MACPORT
  :config
  (mac-pseudo-daemon-mode))

(use-package exec-path-from-shell
  :when IS-MAC
  :config
  (exec-path-from-shell-initialize))

(defun cat-client-frame-config ()
  (select-frame-set-input-focus (selected-frame))
  (when (not cat-font-load)
    (cat! "+font")
    (setq cat-font-load t)))

(add-hook 'server-after-make-frame-hook #'cat-client-frame-config)
