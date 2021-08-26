(defvar cat-default-font "Roboto Mono 16")
(defvar cat-alt-code-font "Cascadia Code")
(defvar cat-cjk-font "LXGW WenKai")
(defvar cat-symbol-fonts nil)
(defvar cat-math-fonts '("Noto Sans Math"))

(when IS-LINUX
  (setq
   cat-symbol-fonts
   '("Noto Color Emoji")))

(when IS-WINDOWS
  (setq
   cat-symbol-fonts
   '("Segoe UI Emoji" "Cambria Math" "Mongolian Baiti" "Segoe UI Symbol")))

(when IS-MAC
  (setq
   cat-default-font "Roboto Mono 18"
   cat-symbol-fonts
   '("Apple Color Emoji" "Arial Unicode MS")))

(set-face-attribute 'default nil :font cat-default-font :weight 'light)

;; 猫，ねこ
(when cat-cjk-font
  (set-fontset-font t 'han (font-spec :family cat-cjk-font))
  (set-fontset-font t 'cjk-misc (font-spec :family cat-cjk-font))
  (set-fontset-font t 'kana (font-spec :family cat-cjk-font))
  (message "Set CJK font %s" cat-cjk-font))

;; 😺 ↩
(when cat-symbol-fonts
  (dolist (font cat-symbol-fonts)
    (set-fontset-font t 'symbol (font-spec :family font)))
  (message "Set Symbol font %s" cat-symbol-fonts))

;; 𝓒𝙖𝕥
(when cat-math-fonts
  (dolist (font cat-math-fonts)
    (set-fontset-font t 'mathematical (font-spec :family font)))
  (message "Set Math font %s" cat-math-fonts))

(setq
 face-font-rescale-alist
 '(("Math" . 0.7)
   ("Noto Sans .+" . 0.7)
   ("Apple Color Emoji" . 0.8)
   ("Noto Serif .+" . 0.9)
   ("Source Han Sans" . 0.9)
   ("-cdac$" . 1.3)))
