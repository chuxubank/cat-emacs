;; -*- lexical-binding: t; -*-

(defvar cat-serif-fonts '("DejaVu Serif" "Roboto Serif")
  "Default proportional serif fonts.")

(defvar cat-slab-fonts '("Iosevka Etoile" "Roboto Slab")
  "Default proportional slab serif fonts.")

(defvar cat-sans-fonts '("Iosevka Aile" "Inter" "DejaVu Sans" "Roboto")
  "Default proportional sans serif fonts.")

(defvar cat-mono-code-fonts '("Victor Mono" "JetBrains Mono" "Cascadia Code" "Fira Code")
  "Default monospaced fonts.")

(defvar cat-mono-thin-fonts '("Iosevka")
  "Default monospaced thin fonts.")

(defvar cat-mono-serif-fonts '("Courier Prime")
  "Default monospaced serif fonts.")

(defvar cat-mono-slab-fonts '("Wellfleet")
  "Default monospaced slab serif fonts.")

(defvar cat-mono-sans-fonts '("DejaVu Sans Mono" "Roboto Mono")
  "Default monospaced sans serif fonts.")

(defvar cat-cjk-mono-fonts '("LXGW WenKai")
  "Font for cjk scripts.")

(defvar cat-math-fonts '("DejaVu Math TeX Gyre" "Noto Sans Math")
  "Fonts for characters in `mathematical' script.")

(defvar cat-default-font (car cat-mono-thin-fonts)
  "Cat default font.

For most causes, we need a 1/2em wide mono font to make UI aligned,
like `org-agenda' and `org-table', as well as make spatial efficient.")

(defvar cat-font-size (cond (IS-MAC 160)
                            (t 140))
  "Cat default font size.")

(defun +safe-set-fontset-fonts (fontset characters font-list &optional frame add)
  "Safely set fontset fonts."
  (when (display-graphic-p)
    (if add
        (dolist (font (ensure-list font-list))
          (if (member font (font-family-list))
              (progn
                (set-fontset-font fontset characters font frame add)
                (message "Set %s fontset font to %s" characters font))
            (warn "Font %s not found" font)))
      (let ((find nil))
        (dolist (font (ensure-list font-list))
          (if (member font (font-family-list))
              (progn (set-fontset-font fontset characters font frame (if find 'append nil))
                     (setq find t)
                     (message "Set %s fontset font to %s" characters font))
            (warn "Font %s not found" font))))))
  )

(defun +safe-set-face-fonts (face font-list &optional frame)
  "Safely set face fonts."
  (when (display-graphic-p)
    (cl-dolist (font (ensure-list font-list))
      (if (member font (font-family-list))
          (progn (set-face-font face font frame)
                 (message "Set %s face font to %s" face font)
                 (cl-return font))
        (warn "Font %s not found" font)))))

(defun +safe-buffer-face-set-fonts (font-list)
  "Safely set buffer face fonts."
  (when (display-graphic-p)
    (cl-dolist (font (ensure-list font-list))
      (if (member font (font-family-list))
          (progn (buffer-face-set `(:family ,font))
                 (message "Set buffer %s face font to %s" (current-buffer) font)
                 (cl-return font))
        (warn "Font %s not found" font)))))

(when (display-graphic-p)
  (if IS-MACPORT
      (set-face-attribute 'default nil :font cat-default-font :height cat-font-size)
    (set-face-attribute 'default nil :font cat-default-font :height cat-font-size :weight 'light)))

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
  :when (display-graphic-p)
  :hook (cat-theme-refresh . nerd-icons-set-font)
  :config
  (defun nerd-icons-set-font (&optional font-family frame)
    "Modify nerd font charsets to use FONT-FAMILY for FRAME."
    (when (display-graphic-p)
      (let ((font-f (or font-family nerd-icons-font-family))
            (charsets '((#xe5fa . #xe6b2)  ;; Seti-UI + Custom
                        (#xe700 . #xe7c5)  ;; Devicons
                        (#xf000 . #xf2e0)  ;; Font Awesome
                        (#xe200 . #xe2a9)  ;; Font Awesome Extension
                        (#xf500 . #xfd46) (#xf0001 . #xf1af0) ;; Material Design Icons
                        (#xe300 . #xe3eb)  ;; Weather
                        (#xf400 . #xf4a9) #x2665 #x26A1  ;; Octicons
                        (#xe0a0 . #xe0a2) (#xe0b0 . #xe0b3)  ;; Powerline Symbols
                        #xe0a3 (#xe0b4 . #xe0c8) #xe0ca (#xe0cc . #xe0d4)  ;; Powerline Extra Symbols
                        (#x23fb . #x23fe) #x2b58  ;; IEC Power Symbols
                        (#xf300 . #xf32d)  ;; Font Logos
                        (#xe000 . #xe00a)  ;; Pomicons
                        (#xea60 . #xebeb))))  ;; Codicons
        (cl-loop for charset in charsets do
                 (set-fontset-font
                  (frame-parameter nil 'font)
                  charset
                  (font-spec :family font-f
                             :weight nil
                             :size   nil)
                  frame
                  'prepend))))))

;; 猫，ねこ，고양이
(+safe-set-fontset-fonts t 'han cat-cjk-mono-fonts)
(+safe-set-fontset-fonts t 'kana cat-cjk-mono-fonts)
(+safe-set-fontset-fonts t 'hangul cat-cjk-mono-fonts)
(+safe-set-fontset-fonts t 'cjk-misc cat-cjk-mono-fonts)

;; 𝓒𝙖𝕥
(+safe-set-fontset-fonts t 'mathematical cat-math-fonts)

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
  "Set font for `org-mode'."
  (+safe-buffer-face-set-fonts cat-mono-sans-fonts))
(add-hook 'org-mode-hook #'cat-setup-org-font)

(defun cat-setup-org-face-font ()
  "Set font for org faces."
  (+safe-set-face-fonts 'org-table cat-mono-thin-fonts)
  (+safe-set-face-fonts 'org-column-title cat-mono-thin-fonts)
  (+safe-set-face-fonts 'org-code cat-mono-code-fonts)
  (+safe-set-face-fonts 'org-block cat-mono-code-fonts)
  (+safe-set-face-fonts 'org-meta-line cat-mono-code-fonts))
(add-hook 'org-load-hook #'cat-setup-org-face-font)

(defun cat-setup-coding-font ()
  "Set font for coding."
  (+safe-buffer-face-set-fonts cat-mono-code-fonts))
(add-hook 'prog-mode-hook #'cat-setup-coding-font)

(defun cat-setup-writing-font()
  "Set font for writing."
  (+safe-buffer-face-set-fonts cat-slab-fonts))
(add-hook 'text-mode-hook #'cat-setup-writing-font)

(defun cat-setup-document-font ()
  "Set font for document."
  (+safe-buffer-face-set-fonts cat-sans-fonts))
(add-hook 'Info-mode-hook #'cat-setup-document-font)

(with-eval-after-load 'face-remap
  (+change-lighter 'buffer-face-mode " 󰛖"))
