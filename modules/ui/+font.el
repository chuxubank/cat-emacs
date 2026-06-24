;; -*- lexical-binding: t; -*-

(defgroup cat-font nil
  "Font settings for Cat Emacs."
  :group 'cat)

(defcustom cat-serif-fonts '("DejaVu Serif" "Roboto Serif")
  "Default proportional serif fonts."
  :type '(repeat string))

(defcustom cat-serif-slab-fonts '("Iosevka Etoile" "Roboto Slab")
  "Default proportional slab serif fonts."
  :type '(repeat string))

(defcustom cat-sans-fonts '("Iosevka Aile"
                            "Inter"
                            "DejaVu Sans"
                            "Roboto"
                            "SF Pro"
                            "HarmonyOS Sans")
  "Default proportional sans serif fonts."
  :type '(repeat string))

(defcustom cat-mono-code-fonts '("Maple Mono"
                                 "JetBrains Mono"
                                 "Cascadia Code"
                                 "Fira Code"
                                 "SF Mono"
                                 "IBM Plex Mono"
                                 "Menlo"
                                 "Monaco"
                                 "Google Sans Code")
  "Default monospaced fonts."
  :type '(repeat string))

(defcustom cat-mono-thin-fonts '("Iosevka Term" "Iosevka")
  "Default monospaced thin fonts."
  :type '(repeat string))

(defcustom cat-mono-serif-fonts '("Courier Prime")
  "Default monospaced serif fonts."
  :type '(repeat string))

(defcustom cat-mono-slab-fonts '("Wellfleet")
  "Default monospaced slab serif fonts."
  :type '(repeat string))

(defcustom cat-mono-sans-fonts '("DejaVu Sans Mono" "Roboto Mono")
  "Default monospaced sans serif fonts."
  :type '(repeat string))

(defcustom cat-cjk-mono-fonts '("LXGW WenKai")
  "Font for cjk scripts."
  :type '(repeat string))

(defcustom cat-math-fonts '("DejaVu Math TeX Gyre" "Noto Sans Math")
  "Fonts for characters in `mathematical' script."
  :type '(repeat string))

(defcustom cat-symbol-fonts '("Apple Symbols")
  "Fonts for symbol characters."
  :type '(repeat string))

(defcustom cat-unicode-fonts '("Apple Color Emoji" "Symbola")
  "Fonts for unicode characters."
  :type '(repeat string))

(defcustom cat-default-font (car cat-mono-thin-fonts)
  "Cat default font.

For most causes, we need a 1/2em wide mono font to make UI aligned,
like `org-agenda' and `org-table', as well as make spatial efficient."
  :type 'string)

(defcustom cat-font-size (cond (IS-MAC 160)
                               (t 140))
  "Cat default font size."
  :type 'integer)

(defcustom cat-fontset-font-rules
  '((unicode cat-unicode-fonts append)
    ((han kana hangul bopomofo cjk-misc) cat-cjk-mono-fonts)
    (mathematical cat-math-fonts))
  "Rules for `set-fontset-font'.
Each rule has the form (CHARACTERS FONTS &optional ADD).  CHARACTERS
can be a script symbol or a list of script symbols.  FONTS can be a
font family, a list of font families, or a symbol whose value is either."
  :type 'sexp)

(defcustom cat-font-profiles
  '((text-sans . cat-sans-fonts)
    (text-serif . cat-serif-fonts)
    (text-serif-slab . cat-serif-slab-fonts)
    (text-mono . cat-mono-sans-fonts)
    (table . cat-mono-thin-fonts)
    (code . cat-mono-code-fonts)
    (code-jvm . ("JetBrains Mono"))
    (code-python . ("Cascadia Code"))
    (code-diagram . ("Fira Code"))
    (code-apple . ("SF Mono"))
    (code-config . ("IBM Plex Mono"))
    (terminal . ("Menlo")))
  "Named font profiles.
Profiles decouple font intent from mode rules.  Each value can be a
font family, a list of font families, or a symbol whose value is either."
  :type 'sexp)

(defcustom cat-mode-font-rules
  `((:modes (org-mode)
            :font text-mono
            :faces ((org-document-title text-serif)
                    (org-table table)
                    (org-formula table)
                    (org-column-title table)
                    (org-code code)
                    (org-block code)
                    (org-meta-line code)))
    (:modes (markdown-mode)
            :font text-mono
            :faces ((markdown-table-face table)
                    (markdown-code-face code)
                    (markdown-inline-code-face code)))
    (:modes (csv-mode)
            :font table)
    (:modes (beancount-mode)
            :font text-mono)
    (:modes (json-mode json-ts-mode
                       yaml-mode yaml-ts-mode
                       toml-ts-mode
                       conf-mode
                       nxml-mode
                       sgml-mode
                       templ-ts-mode
                       go-template-mode)
            :font code-config)
    (:modes (objc-mode swift-mode applescript-mode)
            :font code-apple)
    (:modes (plantuml-mode mermaid-mode mermaid-ts-mode)
            :font code-diagram)
    (:modes (python-base-mode)
            :font code-python)
    (:modes (kotlin-ts-mode kotlin-mode
                            java-ts-mode java-mode
                            js-base-mode
                            typescript-ts-base-mode typescript-mode)
            :font code-jvm)
    (:modes (comint-mode mistty-mode vterm-mode ghostel-mode logview-mode)
            :font terminal
            :rescale (("Symbols Nerd Font" . 1.2)))
    (:modes (prog-mode)
            :font code)
    (:buffer-name "Meow Cheatsheet"
                  :font code)
    (:modes (text-mode)
            :font text-serif-slab)
    (:modes (Info-mode man-common treemacs-mode)
            :font text-sans))
  "Rules for buffer-local font selection.
Each rule is a plist.  Supported keys are:

:modes       A mode or list of modes matched with `derived-mode-p'.
:buffer-name A regexp matched against `buffer-name'.
:font        Font profile, font family, font list, or font variable.
:faces       Face rules in the form (FACE FONTS).
:rescale     Buffer-local `face-font-rescale-alist' value."
  :type 'sexp)

(defvar cat-setup-fonts-hook nil
  "Hook runs after setup fonts.")

(defun cat--font-value (fonts)
  "Return resolved FONTS.
FONTS can be a value, a font profile, or a variable symbol."
  (cond
   ((assq fonts cat-font-profiles)
    (cat--font-value (alist-get fonts cat-font-profiles)))
   ((and (symbolp fonts) (boundp fonts))
    (symbol-value fonts))
   (t fonts)))

(defun cat--font-list (fonts)
  "Return resolved FONTS as a list."
  (ensure-list (cat--font-value fonts)))

(defun +safe-set-fontset-fonts (fontset characters font-list &optional frame add)
  "Safely set fontset fonts.
If ADD is non-nil, all fonts in FONT-LIST are set with given ADD parameter.
If ADD is nil, the first existing font is set as replacement, and others are appended."
  (when (display-graphic-p)
    (let ((fonts (cat--font-list font-list))
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
    (cl-dolist (font (cat--font-list font-list))
      (if (member font (font-family-list))
          (progn (set-face-attribute face frame :family font :inherit 'fixed-pitch)
                 (message "Set %s face font to %s" face font)
                 (cl-return font))
        (warn "Font %s not found" font)))))

(defun +safe-buffer-face-set-fonts (font-list)
  "Safely set buffer face fonts."
  (when (display-graphic-p)
    (cl-dolist (font (cat--font-list font-list))
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
    (pcase-dolist (`(,scripts ,fonts . ,args) cat-fontset-font-rules)
      (dolist (script (ensure-list scripts))
        (+safe-set-fontset-fonts t script fonts frame (car args))))
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

(use-package nerd-icons-completion
  :hook (after-init . nerd-icons-completion-mode))

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

(defun cat--mode-font-rule-matches-p (rule)
  "Return non-nil when RULE applies to the current buffer."
  (or (when-let* ((modes (plist-get rule :modes)))
        (apply #'derived-mode-p (ensure-list modes)))
      (when-let* ((regexp (plist-get rule :buffer-name)))
        (string-match-p regexp (buffer-name)))))

(defun cat--apply-mode-font-rule (rule)
  "Apply a mode font RULE to the current buffer."
  (when-let* ((font (plist-get rule :font)))
    (+safe-buffer-face-set-fonts font))
  (pcase-dolist (`(,face ,fonts) (plist-get rule :faces))
    (+safe-set-face-fonts face fonts))
  (when-let* ((rescale (plist-get rule :rescale)))
    (setq-local face-font-rescale-alist rescale)))

(defun cat-setup-mode-font ()
  "Set font according to current major mode.
Unless `buffer-face-mode' already enabled."
  (unless (bound-and-true-p buffer-face-mode)
    (when-let* ((rule (seq-find #'cat--mode-font-rule-matches-p cat-mode-font-rules)))
      (cat--apply-mode-font-rule rule))))

(add-hook 'window-configuration-change-hook 'cat-setup-mode-font)
(add-hook 'after-revert-hook 'cat-setup-mode-font)
