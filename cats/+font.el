(defvar cat-default-font "Roboto Mono 16")
(defvar cat-symbol-font nil)
(defvar cat-cjk-font nil)
(defvar cat-emoji-font nil)

(when IS-LINUX
  (setq
   cat-cjk-font "Noto Sans CJK SC"
   cat-emoji-font "Noto Color Emoji"))

(when IS-WINDOWS
  (setq
   cat-cjk-font "DengXian"
   cat-emoji-font "Segoe UI Emoji"))

(set-face-attribute 'default nil :font cat-default-font :weight 'light)

(when cat-symbol-font
  (set-fontset-font t 'symbol (font-spec :family cat-symbol-font))
  (message "Set Symbol font %s" cat-symbol-font))

;; 猫
(when cat-cjk-font
  (set-fontset-font t 'han (font-spec :family cat-cjk-font))
  (message "Set CJK font %s" cat-cjk-font))

;; 😺
(when cat-emoji-font
  (set-fontset-font t 'unicode (font-spec :family cat-emoji-font) nil 'append)
  (message "Set Emoji font %s" cat-emoji-font))
