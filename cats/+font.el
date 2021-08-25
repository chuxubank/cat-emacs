(defvar cat-default-font "Roboto Mono 16")
(defvar cat-cjk-font "LXGW WenKai")
(defvar cat-symbol-fonts nil)

(when IS-LINUX
  (setq
   cat-symbol-fonts
   '("Noto Color Emoji")))

(when IS-WINDOWS
  (setq
   cat-symbol-fonts
   '("Segoe UI Emoji" "Cambria Math" "Mongolian Baiti" "Segoe UI Symbol")))

(set-face-attribute 'default nil :font cat-default-font :weight 'light)

;; 猫，ぁ
(when cat-cjk-font
  (set-fontset-font t 'han (font-spec :family cat-cjk-font))
  (set-fontset-font t 'cjk-misc (font-spec :family cat-cjk-font))
  (set-fontset-font t 'kana (font-spec :family cat-cjk-font))
  (message "Set CJK font %s" cat-cjk-font))

;; 😺
(when cat-symbol-fonts
  (dolist (font cat-symbol-fonts)
    (set-fontset-font t 'symbol (font-spec :family font)))
  (message "Set Symbol font %s" cat-symbol-fonts))
