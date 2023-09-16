;; -*- lexical-binding: t; -*-

(defvar cat-font-load nil
  "Whether cat font has been loaded.")

(defun cat-client-frame-config ()
  (select-frame-set-input-focus (selected-frame))
  (when (not cat-font-load)
    (cat! "+font")
    (setq cat-font-load t)))

(add-hook 'server-after-make-frame-hook #'cat-client-frame-config)
