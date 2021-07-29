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
   cat-symbol-font "Segoe UI Symbol"
   cat-cjk-font "LXGW WenKai"
   cat-unicode-fonts '("Segoe UI Emoji" "Cambria Math" "Mongolian Baiti")))

(set-face-attribute 'default nil :font cat-default-font :weight 'light)

;; 猫
(when cat-cjk-font
  (set-fontset-font t 'han (font-spec :family cat-cjk-font))
  (message "Set CJK font %s" cat-cjk-font))

;; 😺
(when cat-unicode-fonts
  (dolist (font cat-unicode-fonts)
    (set-fontset-font t 'unicode (font-spec :family font) nil 'append))
  (message "Set Unicode font %s" cat-unicode-fonts))

(when cat-symbol-font
  (set-fontset-font t 'symbol (font-spec :family cat-symbol-font) nil 'append)
  (message "Set Symbol font %s" cat-symbol-font))
