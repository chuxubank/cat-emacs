;; -*- lexical-binding: t; -*-

(defvar cat-default-font "Roboto Mono 16")
(defvar cat-alt-code-font "Cascadia Code")
(defvar cat-cjk-font "LXGW WenKai")
(defvar cat-mono-font "LXGW WenKai Mono")
(defvar cat-math-fonts '("Noto Sans Math"))
(defvar cat-unicode-fonts '("Apple Symbols" "LXGW WenKai Mono" "Symbols Nerd Font Mono"))

(defun +safe-set-fonts (fontset characters font-name-list &optional frame add)
  (dolist (font (ensure-list font-name-list))
    (when (member font (font-family-list))
      (set-fontset-font fontset characters font frame (or add 'prepend))
      (message "Set %s font to %s" characters font-name-list))))

(when IS-WINDOWS
  (setq cat-default-font "RobotoMono NF 14"))

(set-face-attribute 'default nil :font cat-default-font :weight 'light)
(set-face-attribute 'variable-pitch nil :font cat-alt-code-font)

;; ←─
(+safe-set-fonts t 'unicode cat-unicode-fonts)

;; 猫，ねこ
(+safe-set-fonts t 'han cat-cjk-font)
(+safe-set-fonts t 'cjk-misc cat-cjk-font)
(+safe-set-fonts t 'kana cat-cjk-font)

;; 𝓒𝙖𝕥
(+safe-set-fonts t 'mathematical cat-math-fonts)

(setq
 face-font-rescale-alist
 '(("Noto Serif Thai" . 0.4)
   ("Noto Naskh Arabic" . 0.4)
   ("Math" . 0.7)
   ("Noto Sans .+" . 0.7)
   ("Apple Color Emoji" . 0.8)
   ("Apple Symbols" . 0.9)
   ("Noto Serif .+" . 0.9)
   ("Source Han Sans" . 0.9)
   ("-cdac$" . 1.3)))

(defun cat-setup-org-font ()
  (set-face-font 'org-table cat-mono-font)
  (set-face-font 'org-column-title cat-mono-font))

(add-hook 'org-mode-hook #'cat-setup-org-font)
