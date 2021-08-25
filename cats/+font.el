(defvar cat-default-font "Roboto Mono 16")
(defvar cat-cjk-font nil)
(defvar cat-unicode-fonts nil)
(defvar cat-symbol-fonts nil)

(when IS-LINUX
  (setq
   cat-cjk-font "Sarasa Mono SC"
   cat-unicode-fonts '("Noto Color Emoji")))

(when IS-WINDOWS
  (setq
   cat-cjk-font "LXGW WenKai"
   cat-unicode-fonts '("Segoe UI Emoji" "Cambria Math" "Mongolian Baiti")
   cat-symbol-fonts '("Segoe UI Symbol")))

(set-face-attribute 'default nil :font cat-default-font :weight 'light)

;; 猫，狗
(when cat-cjk-font
  (set-fontset-font t 'han (font-spec :family cat-cjk-font))
  (set-fontset-font t 'cjk-misc (font-spec :family cat-cjk-font))
  (message "Set CJK font %s" cat-cjk-font))

;; 😺
(when cat-unicode-fonts
  (dolist (font cat-unicode-fonts)
    (set-fontset-font t 'unicode (font-spec :family font)))
  (message "Set Unicode font %s" cat-unicode-fonts))

(when cat-symbol-fonts
  (dolist (font cat-symbol-fonts)
    (set-fontset-font t 'symbol (font-spec :family font) nil 'append))
  (message "Set Symbol font %s" cat-symbol-fonts))
