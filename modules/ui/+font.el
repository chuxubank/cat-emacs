;; -*- lexical-binding: t; -*-

(defgroup cat-font nil
  "Font settings for Cat Emacs."
  :group 'cat)

(defcustom cat-cjk-mono-fonts '("LXGW WenKai")
  "Font for cjk scripts."
  :type '(repeat string))

(defcustom cat-math-fonts '("STIX Two Math"
                            "DejaVu Math TeX Gyre"
                            "Noto Sans Math")
  "Fonts for characters in `mathematical' script."
  :type '(repeat string))

(defcustom cat-symbol-fonts '("Apple Symbols")
  "Fonts for symbol characters."
  :type '(repeat string))

(defcustom cat-unicode-fonts '("Apple Color Emoji" "Symbola")
  "Fonts for unicode characters."
  :type '(repeat string))

(defcustom cat-font-family-alternatives
  '(("Iosevka Term" "Iosevka" "DejaVu Sans Mono")
    ("DejaVu Serif" "Roboto Serif" "Georgia")
    ("DejaVu Sans Mono" "Roboto Mono" "Menlo")
    ("Iosevka Etoile" "Roboto Slab" "Charter" "DejaVu Serif")
    ("Iosevka Aile" "Inter" "Avenir Next" "DejaVu Sans")
    ("Maple Mono" "JetBrains Mono" "Cascadia Code" "Fira Code"
     "SF Mono" "IBM Plex Mono" "Menlo" "DejaVu Sans Mono")
    ("Big Caslon" "Iowan Old Style" "Hoefler Text" "Charter"
     "Georgia")
    ("Iowan Old Style" "Charter" "Hoefler Text" "Georgia"
     "DejaVu Serif")
    ("DIN Condensed" "Avenir Next Condensed" "Avenir Next" "Inter"
     "DejaVu Sans")
    ("STIX Two Text" "Charter" "Roboto Serif" "DejaVu Serif")
    ("Avenir Next" "Inter" "Optima" "DejaVu Sans")
    ("Charter" "Iowan Old Style" "Georgia" "DejaVu Serif")
    ("SF Mono" "Menlo" "JetBrains Mono" "DejaVu Sans Mono")
    ("Menlo" "SF Mono" "DejaVu Sans Mono")
    ("JetBrains Mono" "Maple Mono" "Cascadia Code" "DejaVu Sans Mono")
    ("Cascadia Code" "JetBrains Mono" "Maple Mono" "DejaVu Sans Mono")
    ("Fira Code" "JetBrains Mono" "Maple Mono" "DejaVu Sans Mono")
    ("IBM Plex Mono" "JetBrains Mono" "Maple Mono" "DejaVu Sans Mono"))
  "Alternative families used when a requested face family is unavailable.
Each entry has the form (FAMILY ALTERNATIVE...).  Missing glyphs are
handled separately by `cat-fontset-font-rules'."
  :type '(repeat (repeat string)))

(defcustom cat-font-specialized-roles
  '((code-jvm :extends code :family "JetBrains Mono")
    (code-python :extends code :family "Cascadia Code")
    (code-diagram :extends code :family "Fira Code")
    (code-apple :extends code :family "SF Mono")
    (code-config :extends code :family "IBM Plex Mono")
    (terminal :extends mono :family "Menlo"))
  "Specialized font roles shared by every preset.
Each role uses :extends to inherit size and weight from a base role."
  :type 'sexp)

(defcustom cat-font-preset 'classic
  "Active font preset."
  :type 'symbol)

(defcustom cat-font-presets
  `((classic
     (default :family "Iosevka Term"
              :height ,(if IS-MAC 160 140) :weight regular)
     (heading :family "DejaVu Serif"
              :weight semi-bold)
     (title :extends heading :height 1.4)
     (heading-1 :extends heading :height 1.25)
     (heading-2 :extends heading :height 1.15)
     (heading-3 :extends heading :height 1.08)
     (body :family "DejaVu Sans Mono"
           :height 1.0 :weight regular)
     (prose :family "Iosevka Etoile"
            :height 1.0 :weight regular)
     (ui :family "Iosevka Aile"
         :height 1.0 :weight regular)
     (mono :family "DejaVu Sans Mono"
           :height 1.0 :weight regular)
     (code :family "Maple Mono"
           :height 1.0 :weight regular)
     (table :family "Iosevka Term"
            :height 1.0 :weight regular))
    (artistic
     (default :family "Iosevka Term"
              :height ,(if IS-MAC 160 140) :weight regular)
     (title :family "Big Caslon" :height 1.55 :weight medium)
     (heading :family "Iowan Old Style" :weight bold)
     (heading-1 :extends heading :height 1.25)
     (heading-2 :extends heading :height 1.15)
     (heading-3 :extends heading :height 1.08)
     (body :family "Iowan Old Style" :height 1.0 :weight regular)
     (prose :extends body)
     (ui :family "Avenir Next" :height 1.0 :weight regular)
     (mono :family "Menlo" :height 1.0 :weight regular)
     (code :extends mono)
     (table :family "Iosevka Term" :height 1.0 :weight regular))
    (technical
     (default :family "Iosevka Term"
              :height ,(if IS-MAC 160 140) :weight regular)
     (title :family "DIN Condensed" :height 1.5 :weight bold)
     (heading :family "Avenir Next" :weight semi-bold)
     (heading-1 :extends heading :height 1.25)
     (heading-2 :extends heading :height 1.15)
     (heading-3 :extends heading :height 1.08)
     (body :family "STIX Two Text" :height 1.0 :weight regular)
     (prose :extends body)
     (ui :family "Avenir Next" :height 1.0 :weight regular)
     (mono :family "SF Mono" :height 1.0 :weight regular)
     (code :extends mono)
     (table :extends mono))
    (modern
     (default :family "Iosevka Term"
              :height ,(if IS-MAC 160 140) :weight regular)
     (heading :family "Avenir Next" :weight semi-bold)
     (title :extends heading :height 1.5 :weight bold)
     (heading-1 :extends heading :height 1.25)
     (heading-2 :extends heading :height 1.15)
     (heading-3 :extends heading :height 1.08)
     (body :family "Avenir Next" :height 1.0 :weight regular)
     (prose :extends body)
     (ui :extends body)
     (mono :family "SF Mono" :height 1.0 :weight regular)
     (code :extends mono)
     (table :extends mono))
    (mono-editorial
     (default :family "Iosevka Term"
              :height ,(if IS-MAC 160 140) :weight regular)
     (heading :family "Charter" :weight bold)
     (title :extends heading :height 1.5 :weight regular)
     (heading-1 :extends heading :height 1.25)
     (heading-2 :extends heading :height 1.15)
     (heading-3 :extends heading :height 1.08)
     (body :family "Iosevka Term" :height 1.0 :weight regular)
     (prose :extends body)
     (ui :family "Avenir Next" :height 0.95 :weight regular)
     (mono :family "Iosevka Term" :height 1.0 :weight regular)
     (code :family "Maple Mono" :height 0.95 :weight regular)
     (table :extends mono)))
  "Typography presets organized by semantic role.
Each role has the form (ROLE :family FAMILY &rest ATTRIBUTES).  The
`default' role uses an absolute face height in tenths of a point;
content roles use heights relative to it.  Family availability fallback
is configured separately by `cat-font-family-alternatives'."
  :type 'sexp)

(defcustom cat-fontset-font-rules
  '((unicode cat-unicode-fonts append)
    ((han kana hangul bopomofo cjk-misc) cat-cjk-mono-fonts)
    (mathematical cat-math-fonts))
  "Rules for `set-fontset-font'.
Each rule has the form (CHARACTERS FONTS &optional ADD).  CHARACTERS
can be a script symbol or a list of script symbols.  FONTS can be a
font family, a list of font families, or a symbol whose value is either."
  :type 'sexp)

(defcustom cat-mode-font-rules
  `((:modes (org-mode)
            :font body
            :faces ((org-document-title title)
                    (org-level-1 heading-1)
                    (org-level-2 heading-2)
                    (org-level-3 heading-3)
                    (org-table table)
                    (org-formula table)
                    (org-column-title table)
                    (org-code code)
                    (org-block code)
                    (org-meta-line code)))
    (:modes (markdown-mode)
            :font body
            :faces ((markdown-header-face heading)
                    (markdown-header-face-1 title)
                    (markdown-header-face-2 heading-1)
                    (markdown-header-face-3 heading-2)
                    (markdown-header-face-4 heading-3)
                    (markdown-table-face table)
                    (markdown-code-face code)
                    (markdown-inline-code-face code)))
    (:modes (csv-mode)
            :font table)
    (:modes (beancount-mode)
            :font mono)
    (:modes (json-mode json-ts-mode
                       yaml-mode yaml-ts-mode
                       toml-ts-mode
                       conf-mode
                       nxml-mode
                       sgml-mode
                       templ-ts-mode
                       go-template-ts-mode
                       jinja2-ts-mode)
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
            :font prose)
    (:modes (Info-mode man-common treemacs-mode)
            :font ui))
  "Rules for buffer-local font selection.
Each rule is a plist.  Supported keys are:

:modes       A mode or list of modes matched with `derived-mode-p'.
:buffer-name A regexp matched against `buffer-name'.
:font        Font role, family, list, or font variable.
:faces       Face rules in the form (FACE FONTS-OR-ROLE &rest ATTRIBUTES).
             Role attributes are merged with rule ATTRIBUTES.
:rescale     Buffer-local `face-font-rescale-alist' value."
  :type 'sexp)

(defvar cat-setup-fonts-hook nil
  "Hook runs after setup fonts.")

(defvar cat-font-preset-change-hook nil
  "Hook run after selecting a font preset.")

(defun cat--merge-font-attributes (base overrides)
  "Return face attributes from BASE with OVERRIDES applied."
  (let ((attributes (copy-sequence base)))
    (while overrides
      (setq attributes
            (plist-put attributes (pop overrides) (pop overrides))))
    attributes))

(defun cat--font-role-spec (role)
  "Return the font specification for ROLE."
  (when (symbolp role)
    (let ((shared (alist-get role cat-font-specialized-roles))
          (overrides
           (alist-get role (alist-get cat-font-preset cat-font-presets))))
      (let* ((spec (cat--merge-font-attributes shared overrides))
             (parent (plist-get spec :extends)))
        (if parent
            (cat--merge-font-attributes (cat--font-role-spec parent) spec)
          spec)))))

(defun cat--install-font-family-alternatives ()
  "Install Cat family alternatives without replacing unrelated entries."
  (dolist (entry cat-font-family-alternatives)
    (setf (alist-get (car entry) face-font-family-alternatives
                     nil nil #'string-equal)
          (copy-sequence (cdr entry)))))

(cat--install-font-family-alternatives)

(defun cat--font-value (fonts)
  "Return resolved FONTS.
FONTS can be a value, a font role, or a variable symbol."
  (let ((spec (cat--font-role-spec fonts)))
    (cond
     (spec
      (cat--font-value (or (plist-get spec :family)
                           (plist-get spec :fonts))))
     ((and (symbolp fonts) (boundp fonts))
      (symbol-value fonts))
     (t fonts))))

(defun cat--font-list (fonts)
  "Return resolved FONTS as a list."
  (let ((value (cat--font-value fonts)))
    (cond
     ((null value) nil)
     ((stringp value) (list value))
     ((and (listp value) (seq-every-p #'stringp value)) value)
     (t (error "Invalid font value: %S" value)))))

(defun cat--font-role-attributes (role)
  "Return face attributes associated with font ROLE."
  (cl-loop for (attribute value) on (cat--font-role-spec role) by #'cddr
           unless (memq attribute '(:family :fonts :extends))
           append (list attribute value)))

(defun cat--font-family-candidates (fonts)
  "Return FONTS expanded with family availability alternatives."
  (delete-dups
   (cl-loop for family in (cat--font-list fonts)
            append
            (cons family
                  (alist-get family cat-font-family-alternatives
                             nil nil #'string-equal)))))

(defun cat--first-existing-font (fonts &optional frame)
  "Return the first font from FONTS available on FRAME."
  (when (display-graphic-p frame)
    (let* ((candidates (cat--font-family-candidates fonts))
           (families (font-family-list frame))
           (font (seq-find (lambda (candidate)
                             (member candidate families))
                           candidates)))
      (unless font
        (warn "No candidate font found: %s"
              (string-join candidates ", ")))
      font)))

(defun cat--face-family (fonts &optional frame)
  "Return the nominal face family for FONTS.
Direct candidate lists are resolved to an installed family on FRAME."
  (let ((value (cat--font-value fonts)))
    (if (stringp value)
        value
      (cat--first-existing-font value frame))))

(defun cat--resolved-face-spec (fonts &optional overrides)
  "Return a face spec for FONTS with role attributes and OVERRIDES."
  (let ((attributes
         (cat--merge-font-attributes
          (cat--font-role-attributes fonts) overrides)))
    (when-let* ((family (cat--face-family fonts)))
      (setq attributes (plist-put attributes :family family)))
    attributes))

(defun +safe-set-fontset-fonts (fontset characters font-list &optional frame add)
  "Safely set fontset fonts.
If ADD is non-nil, all fonts in FONT-LIST are set with given ADD parameter.
If ADD is nil, the first existing font is set as replacement, and others are appended."
  (when (display-graphic-p frame)
    (let ((fonts (cat--font-list font-list))
          (families (font-family-list frame))
          (first-set nil))
      (dolist (font fonts)
        (if (member font families)
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


(defun +safe-set-face-fonts (face fonts &optional frame)
  "Safely set FACE family from FONTS or a font role."
  (when-let* ((family (cat--face-family fonts frame)))
    (set-face-attribute face frame :family family :inherit 'fixed-pitch)
    (message "Set %s face font to %s" face family)
    family))

(defun +safe-buffer-face-set-fonts (fonts)
  "Safely set the current buffer face from FONTS or a font role."
  (when-let* ((spec (cat--resolved-face-spec fonts)))
    (buffer-face-set spec)
    (message "Set buffer %s face to %s" (current-buffer) fonts)
    spec))

(defun cat-setup-fonts (&optional frame)
  "Set fonts on FRAME for Cat Emacs."
  (cat--install-font-family-alternatives)
  (when (display-graphic-p frame)
    (cat-benchmark 'beg "setup fonts.")
    (when-let* ((family (cat--face-family 'default frame)))
      (apply #'set-face-attribute 'default frame :family family
             (cat--font-role-attributes 'default)))
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

(defvar-local cat--mode-font-state nil
  "Font preset state last applied to the current buffer.")

(defvar-local cat--mode-face-remap-cookies nil
  "Face remapping cookies owned by Cat in the current buffer.")

(defvar-local cat--mode-buffer-face nil
  "Buffer face specification owned by Cat in the current buffer.")

(defvar-local cat--mode-font-rescale-state nil
  "Previous buffer-local rescale state saved by Cat.")

(defun cat--clear-mode-font ()
  "Remove mode font settings owned by Cat from the current buffer."
  (mapc #'face-remap-remove-relative cat--mode-face-remap-cookies)
  (setq cat--mode-face-remap-cookies nil)
  (when cat--mode-buffer-face
    (when (equal buffer-face-mode-face cat--mode-buffer-face)
      (buffer-face-set))
    (setq cat--mode-buffer-face nil))
  (when cat--mode-font-rescale-state
    (pcase-let ((`(,local-p ,value) cat--mode-font-rescale-state))
      (if local-p
          (setq-local face-font-rescale-alist value)
        (kill-local-variable 'face-font-rescale-alist)))
    (setq cat--mode-font-rescale-state nil)))

(defun cat--apply-mode-font-rule (rule)
  "Apply a mode font RULE to the current buffer."
  (cat--clear-mode-font)
  (when-let* ((font (plist-get rule :font)))
    (when-let* ((spec (+safe-buffer-face-set-fonts font)))
      (setq cat--mode-buffer-face spec)))
  (pcase-dolist (`(,face ,fonts . ,attributes) (plist-get rule :faces))
    (when-let* ((spec (cat--resolved-face-spec fonts attributes)))
      (push (face-remap-add-relative face spec)
            cat--mode-face-remap-cookies)))
  (when-let* ((rescale (plist-get rule :rescale)))
    (setq cat--mode-font-rescale-state
          (list (local-variable-p 'face-font-rescale-alist)
                face-font-rescale-alist))
    (setq-local face-font-rescale-alist rescale)))

(defun cat-setup-mode-font (&optional force)
  "Set fonts according to the current major mode.
With FORCE, reapply the active preset.  Respect a `buffer-face-mode'
owned by other configuration."
  (unless (and (bound-and-true-p buffer-face-mode)
               (not (equal buffer-face-mode-face cat--mode-buffer-face)))
    (let* ((rule (seq-find #'cat--mode-font-rule-matches-p
                           cat-mode-font-rules))
           (state (list cat-font-preset major-mode rule)))
      (when (or force (not (equal state cat--mode-font-state)))
        (if rule
            (cat--apply-mode-font-rule rule)
          (cat--clear-mode-font))
        (setq cat--mode-font-state state)))))

(defun cat-refresh-mode-fonts ()
  "Reapply the active font preset in every live buffer."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (cat-setup-mode-font t)))))

(defun cat-set-font-preset (preset)
  "Select font PRESET and refresh live buffers."
  (interactive
   (list (intern
          (completing-read "Font preset: "
                           (mapcar #'car cat-font-presets)
                           nil t nil nil
                           (symbol-name cat-font-preset)))))
  (unless (assq preset cat-font-presets)
    (user-error "Unknown font preset: %s" preset))
  (setq cat-font-preset preset)
  (mapc #'cat-setup-fonts (frame-list))
  (run-hooks 'cat-font-preset-change-hook)
  (cat-refresh-mode-fonts)
  (message "Selected %s font preset" preset))

(add-hook 'window-configuration-change-hook 'cat-setup-mode-font)
(add-hook 'after-change-major-mode-hook 'cat-setup-mode-font)
(add-hook 'after-revert-hook 'cat-setup-mode-font)
