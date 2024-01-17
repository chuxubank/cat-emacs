;; -*- lexical-binding: t; -*-

(defvar cat-text-font (if IS-WINDOWS "RobotoMono NF" "Roboto Mono")
  "Font for `text-mode'.")

(defvar cat-code-font "JetBrains Mono"
  "Font for `prog-mode'.")

(defvar cat-cjk-font "LXGW WenKai"
  "Font for cjk scripts.")

(defvar cat-mono-font "Sarasa Term SC Nerd"
  "Font for faces need monospaced font.")

(defvar cat-font-size (cond (IS-MAC 160)
                            (t 140))
  "Font size.")

(defvar cat-math-fonts '("Noto Sans Math")
  "Fonts for characters in `mathematical' script.")

(defun +safe-set-fonts (fontset characters font-name-list &optional frame add)
  (dolist (font (ensure-list font-name-list))
    (when (member font (font-family-list))
      (set-fontset-font fontset characters font frame add)
      (message "Set %s font to %s" characters font-name-list))))

;; Cat UI
;; For most causes, we need a mono font to make UI aligned, like `org-agenda' and `org-table'
(if IS-MACPORT
    (set-face-attribute 'default nil :font cat-mono-font :height cat-font-size)
  (set-face-attribute 'default nil :font cat-mono-font :height cat-font-size :weight 'light))

;; Ligature support
(if IS-MACPORT
    (mac-auto-operator-composition-mode)
  (use-package ligature
    :config
    ;; Enable the "www" ligature in every possible major mode
    (ligature-set-ligatures 't '("www"))
    ;; Enable traditional ligature support in eww-mode, if the
    ;; `variable-pitch' face supports it
    (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
    ;; Enable all Cascadia Code ligatures in programming modes
    (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                         ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                         "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                         "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                         "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                         "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                         "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                         "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                         ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                         "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                         "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                         "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                         "\\\\" "://"))
    ;; Enables ligature checks globally in all buffers. You can also do it
    ;; per mode with `ligature-mode'.
    (global-ligature-mode t)))

;; 󰄛
(use-package nerd-icons
  :config
  (nerd-icons-set-font))

;; 猫，ねこ，고양이
(+safe-set-fonts t 'han cat-cjk-font)
(+safe-set-fonts t 'kana cat-cjk-font)
(+safe-set-fonts t 'hangul cat-cjk-font)
(+safe-set-fonts t 'cjk-misc cat-cjk-font)

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

(defun cat-setup-code-font ()
  (buffer-face-set `(:family ,cat-code-font)))
(add-hook 'prog-mode-hook #'cat-setup-code-font)

(defun cat-setup-text-font ()
  (buffer-face-set `(:family ,cat-text-font)))
(add-hook 'text-mode-hook #'cat-setup-text-font)
