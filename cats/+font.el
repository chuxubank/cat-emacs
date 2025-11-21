;; -*- lexical-binding: t; -*-

(defvar cat-serif-fonts '("DejaVu Serif" "Roboto Serif")
  "Default proportional serif fonts.")

(defvar cat-slab-fonts '("Iosevka Etoile" "Roboto Slab")
  "Default proportional slab serif fonts.")

(defvar cat-sans-fonts '("Iosevka Aile" "Inter" "DejaVu Sans" "Roboto")
  "Default proportional sans serif fonts.")

(defvar cat-mono-code-fonts '("Maple Mono"
                              "JetBrains Mono"
                              "Cascadia Code"
                              "Fira Code"
                              "SF Mono"
                              "IBM Plex Mono"
                              "Menlo"
                              "Monaco")
  "Default monospaced fonts.")

(defvar cat-mono-thin-fonts '("Iosevka Term" "Iosevka")
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

(defvar cat-symbol-fonts '("Apple Symbols"))

(defvar cat-unicode-fonts '("Apple Color Emoji" "Symbola"))

(defvar cat-default-font (car cat-mono-thin-fonts)
  "Cat default font.

For most causes, we need a 1/2em wide mono font to make UI aligned,
like `org-agenda' and `org-table', as well as make spatial efficient.")

(defvar cat-font-size (cond (IS-MAC 160)
                            (t 140))
  "Cat default font size.")

(defvar cat-setup-fonts-hook nil
  "Hook runs after setup fonts.")

(defun +safe-set-fontset-fonts (fontset characters font-list &optional frame add)
  "Safely set fontset fonts.
If ADD is non-nil, all fonts in FONT-LIST are set with given ADD parameter.
If ADD is nil, the first existing font is set as replacement, and others are appended."
  (when (display-graphic-p)
    (let ((fonts (ensure-list font-list))
          (first-set nil))
      (dolist (font fonts)
        (if (member font (font-family-list))
            (progn
              (set-fontset-font
               fontset characters font frame
               (cond
                (add add) ; use whatever was passed in
                (first-set 'append) ; already set one => append
                (t nil))) ; first time => replace
              (setq first-set t)
              (message "Set %s fontset font to %s" characters font))
          (warn "Font %s not found" font))))))


(defun +safe-set-face-fonts (face font-list &optional frame)
  "Safely set face fonts."
  (when (display-graphic-p)
    (cl-dolist (font (ensure-list font-list))
      (if (member font (font-family-list))
          (progn (set-face-attribute face frame :family font :inherit 'fixed-pitch)
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

(defun cat-setup-fonts (&optional frame)
  "Set fonts on FRAME for Cat Emacs."
  (when (display-graphic-p)
    (cat-benchmark 'beg "setup fonts.")
    (if IS-MACPORT
        (set-face-attribute 'default frame :font cat-default-font :height cat-font-size)
      (set-face-attribute 'default frame :font cat-default-font :height cat-font-size :weight 'light))
    ;; (set-face-attribute 'mode-line-active frame :font (cadr cat-mono-thin-fonts))
    ;; (set-face-attribute 'mode-line-inactive frame :font (cadr cat-mono-thin-fonts))

    ;; (+safe-set-fontset-fonts t 'symbol cat-symbol-fonts frame)

    ;; 🐾
    (+safe-set-fontset-fonts t 'unicode cat-unicode-fonts frame 'append)

    ;; 猫，ネコ，ねこ，고양이，ㄇㄠ
    (+safe-set-fontset-fonts t 'han cat-cjk-mono-fonts frame)
    (+safe-set-fontset-fonts t 'kana cat-cjk-mono-fonts frame)
    (+safe-set-fontset-fonts t 'hangul cat-cjk-mono-fonts frame)
    (+safe-set-fontset-fonts t 'bopomofo cat-cjk-mono-fonts frame)
    (+safe-set-fontset-fonts t 'cjk-misc cat-cjk-mono-fonts frame)

    ;; 𝓒𝙖𝕥
    (+safe-set-fontset-fonts t 'mathematical cat-math-fonts frame)

    ;; 󰄛
    (run-hook-with-args 'cat-setup-fonts-hook nil frame)
    (cat-benchmark 'end "setup fonts.")))

(add-hook 'cat-theme-refresh-hook #'cat-setup-fonts)

(if IS-MACPORT
    (mac-auto-operator-composition-mode)
  (use-package ligature
    :hook (after-init . global-ligature-mode)
    :config
    ;; Enable the "www" ligature in every possible major mode
    (ligature-set-ligatures 't '("www"
                                 "[TODO]" "todo))"
                                 "[FIXME]" "fixme))"
                                 "[DEBUG]" "[INFO]" "[WARN]" "[ERROR]"))
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
                                         "\\\\" "://"))))

(use-package nerd-icons
  :hook (cat-setup-fonts . nerd-icons-set-font))

(setq
 face-font-rescale-alist
 '(("Noto Serif Thai" . 0.4)
   ("Noto Naskh Arabic" . 0.4)
   ("Math" . 0.7)
   ("Noto Sans .+" . 0.7)
   ("Apple Color Emoji" . 0.8)
   ("Sinhala Sangam MN". 0.8)
   ("Apple Symbols" . 0.9)
   ("Noto Serif .+" . 0.9)
   ("Source Han Sans" . 0.9)
   ("-cdac$" . 1.3)))

(defun cat-setup-mode-font ()
  "Set font according to current major mode.
Unless `buffer-face-mode' already enabled."
  (unless (bound-and-true-p buffer-face-mode)
    (cond
     ((derived-mode-p 'org-mode)
      (+safe-buffer-face-set-fonts cat-mono-sans-fonts)
      (+safe-set-face-fonts 'org-table cat-mono-thin-fonts)
      (+safe-set-face-fonts 'org-formula cat-mono-thin-fonts)
      (+safe-set-face-fonts 'org-column-title cat-mono-thin-fonts)
      (+safe-set-face-fonts 'org-code cat-mono-code-fonts)
      (+safe-set-face-fonts 'org-block cat-mono-code-fonts)
      (+safe-set-face-fonts 'org-meta-line cat-mono-code-fonts))
     ((derived-mode-p 'markdown-mode)
      (+safe-buffer-face-set-fonts cat-mono-sans-fonts)
      (+safe-set-face-fonts 'markdown-table-face cat-mono-thin-fonts)
      (+safe-set-face-fonts 'markdown-code-face cat-mono-code-fonts)
      (+safe-set-face-fonts 'markdown-inline-code-face cat-mono-code-fonts))
     ((derived-mode-p 'beancount-mode)
      (+safe-buffer-face-set-fonts cat-mono-sans-fonts))
     ((derived-mode-p 'json-mode 'json-ts-mode
                      'yaml-mode 'yaml-ts-mode
                      'toml-ts-mode
                      'conf-mode
                      'nxml-mode
                      'sgml-mode
                      'templ-ts-mode
                      'go-template-mode)
      (+safe-buffer-face-set-fonts (nth 5 cat-mono-code-fonts)))
     ((derived-mode-p 'objc-mode
                      'swift-mode
                      'applescript-mode)
      (+safe-buffer-face-set-fonts (nth 4 cat-mono-code-fonts)))
     ((derived-mode-p 'plantuml-mode
                      'mermaid-mode 'mermaid-ts-mode)
      (+safe-buffer-face-set-fonts (nth 3 cat-mono-code-fonts)))
     ((derived-mode-p 'python-base-mode)
      (+safe-buffer-face-set-fonts (nth 2 cat-mono-code-fonts)))
     ((derived-mode-p 'kotlin-ts-mode 'kotlin-mode
                      'java-ts-mode 'java-mode
                      'js-base-mode
                      'typescript-ts-base-mode 'typescript-mode)
      (+safe-buffer-face-set-fonts (nth 1 cat-mono-code-fonts)))
     ((derived-mode-p 'comint-mode
                      'mistty-mode
                      'vterm-mode
                      'logview-mode)
      (let ((font (nth 0 cat-mono-code-fonts)))
        (+safe-buffer-face-set-fonts font)
        (setq-local face-font-rescale-alist
                    '(("Symbols Nerd Font" . 1.2)))))
     ((derived-mode-p 'prog-mode)
      (+safe-buffer-face-set-fonts cat-mono-code-fonts))
     ((string-match-p "Meow Cheatsheet" (buffer-name))
      (+safe-buffer-face-set-fonts cat-mono-code-fonts))
     ((derived-mode-p 'text-mode)
      (+safe-buffer-face-set-fonts cat-slab-fonts))
     ((derived-mode-p 'Info-mode
                      'man-common)
      (+safe-buffer-face-set-fonts cat-sans-fonts)))))

(add-hook 'window-configuration-change-hook 'cat-setup-mode-font)
(add-hook 'after-revert-hook 'cat-setup-mode-font)

(with-eval-after-load 'face-remap
  (+change-lighter 'buffer-face-mode " 󰛖"))
