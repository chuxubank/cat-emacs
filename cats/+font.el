;; -*- lexical-binding: t; -*-

(defvar cat-default-font "Roboto Mono 16")
(defvar cat-alt-code-font "Cascadia Code")
(defvar cat-cjk-font "LXGW WenKai")
(defvar cat-mono-font "LXGW WenKai Mono")
(defvar cat-math-fonts '("Noto Sans Math"))

(when IS-MAC
  (setq cat-default-font "Roboto Mono 18"))

(when IS-WINDOWS
  (setq cat-default-font "RobotoMono NF 14"))

(set-face-attribute 'default nil :font cat-default-font :weight 'light)

;; 猫，ねこ
(when cat-cjk-font
  (set-fontset-font t 'han (font-spec :family cat-cjk-font))
  (set-fontset-font t 'cjk-misc (font-spec :family cat-cjk-font))
  (set-fontset-font t 'kana (font-spec :family cat-cjk-font))
  (message "Set CJK font %s" cat-cjk-font))

;; 𝓒𝙖𝕥
(when cat-math-fonts
  (dolist (font cat-math-fonts)
    (set-fontset-font t 'mathematical (font-spec :family font)))
  (message "Set Math font %s" cat-math-fonts))

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
