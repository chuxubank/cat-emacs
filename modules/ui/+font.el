;; -*- lexical-binding: t; -*-

(defgroup cat-font nil
  "Font settings for Cat Emacs."
  :group 'cat)

(defcustom cat-font-family-cjk-alist
  '(("Sans Serif UI" . "CJK Sans Serif")
    ("Serif" . "CJK Serif")
    ("Slab Serif" . "CJK Serif")
    ("Quasi Proportional" . "CJK Serif")
    ("Monospace Narrow" . "CJK Monospace")
    ("Monospace Code" . "CJK Monospace")
    ("Monospace Sans Serif" . "CJK Monospace"))
  "CJK logical family associated with each logical font family."
  :type '(alist :key-type string :value-type string))

(defconst cat-cjk-scripts '(han kana hangul bopomofo cjk-misc)
  "Scripts configured by Cat's CJK font rules.")

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

(defcustom cat-font-preset
  `((default :family "Monospace Narrow"
             :height ,(if IS-MAC 160 140))
    (heading :family "Serif" :weight semi-bold)
    (title :extends heading :height 1.4)
    (body :family "Monospace Sans Serif")
    (documentation :extends body)
    (prose :family "Quasi Proportional")
    (ui :family "Sans Serif UI")
    (metadata-label :extends ui)
    (metadata-value :family "Monospace Narrow")
    (mono :family "Monospace Sans Serif")
    (code :family "Monospace Code")
    (table :extends mono :family "Monospace Narrow")
    (code-jvm :extends code :fonts ("JetBrains Mono"))
    (code-python :extends code :fonts ("Cascadia Code"))
    (code-diagram :extends code :fonts ("Fira Code"))
    (code-apple :extends code :fonts ("SF Mono"))
    (code-config :extends code :fonts ("IBM Plex Mono"))
    (terminal :extends mono :fonts ("Menlo")))
  "Typography preset organized by semantic role.
Each role has the form (ROLE :family FAMILY &rest ATTRIBUTES).  FAMILY
should normally name a logical family in
`face-font-family-alternatives'.  The
`default' role uses an absolute face height in tenths of a point;
content roles use heights relative to it.  A role can use :extends and
:fonts to prepend concrete families to the inherited logical family."
  :type 'sexp)

(defcustom cat-fontset-font-rules
  '((unicode cat-unicode-fonts append)
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
                    (org-level-* heading)
                    (org-table table)
                    (org-formula table)
                    (org-column-title table)
                    (org-code code)
                    (org-block code)
                    (org-meta-line code)
                    (org-special-keyword metadata-label)
                    (org-drawer metadata-label)
                    (org-todo metadata-label)
                    (org-done metadata-label)
                    (org-date metadata-value)
                    (org-property-value metadata-value)))
    (:modes (markdown-mode)
            :font body
            :faces ((markdown-header-face heading)
                    (markdown-header-face-1 title)
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
    (:modes (Info-mode man-common)
            :font documentation)
    (:modes (treemacs-mode)
            :font ui))
  "Rules for buffer-local font selection.
Each rule is a plist.  Supported keys are:

:modes       A mode or list of modes matched with `derived-mode-p'.
:buffer-name A regexp matched against `buffer-name'.
:font        Font role, family, list, or font variable.
:faces       Face rules in the form (FACE FONTS-OR-ROLE &rest ATTRIBUTES).
             A FACE ending in * matches every face with that prefix.
             Role attributes are merged with rule ATTRIBUTES.
:rescale     Buffer-local `face-font-rescale-alist' value."
  :type 'sexp)

(defvar cat-setup-fonts-hook nil
  "Hook runs after setup fonts.")

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
    (let* ((spec (alist-get role cat-font-preset))
           (parent (plist-get spec :extends)))
      (if parent
          (cat--merge-font-attributes (cat--font-role-spec parent) spec)
        spec))))

(defun cat--font-value (fonts)
  "Return resolved FONTS.
FONTS can be a value, a font role, or a variable symbol."
  (let ((spec (cat--font-role-spec fonts)))
    (cond
     (spec
      (let ((preferred (plist-get spec :fonts))
            (family (plist-get spec :family)))
        (if preferred
            (delete-dups
             (append (cat--font-list preferred)
                     (cat--font-list family)))
          (cat--font-value family))))
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

(defun cat--font-role-face (role)
  "Return the face owned by font ROLE, creating it when necessary."
  (when (cat--font-role-spec role)
    (let ((face (intern (format "cat-font-role-%s" role))))
      (unless (facep face)
        (make-empty-face face))
      face)))

(defun cat--configure-font-role-face (role &optional frame fontset)
  "Configure ROLE's face on FRAME, optionally using FONTSET."
  (let ((face (cat--font-role-face role))
        (family (plist-get (cat--font-role-spec role) :family)))
    ;; `:font' resolves a fontset to its Latin font and drops script mappings.
    (apply #'set-face-attribute face frame
           (append (list :family family)
                   (when fontset (list :fontset fontset))
                   (cat--font-role-attributes role)))
    face))

(dolist (role cat-font-preset)
  (cat--configure-font-role-face (car role)))

(defun cat--font-family-candidates (fonts)
  "Return FONTS expanded with family availability alternatives."
  (delete-dups
   (cl-loop for family in (cat--font-list fonts)
            append
            (cons family
                  (alist-get family face-font-family-alternatives
                             nil nil #'string-equal)))))

(defvar cat--fontset-signatures (make-hash-table :test 'eq)
  "Last configured signature for each Cat role fontset.")

(defun cat--fontset-name (role)
  "Return the fontset name owned by ROLE."
  (format "-*-cat-*-*-*-*-*-*-*-*-*-*-fontset-cat_%s"
          (replace-regexp-in-string "-" "_" (symbol-name role))))

(defun cat--font-role-cjk-candidates (role)
  "Return CJK candidates selected by ROLE's logical family."
  (let* ((family (plist-get (cat--font-role-spec role) :family))
         (cjk-family (alist-get family cat-font-family-cjk-alist
                                nil nil #'string-equal))
         (candidates (and cjk-family
                          (alist-get cjk-family
                                     face-font-family-alternatives
                                     nil nil #'string-equal))))
    (unless candidates
      (error "No CJK alternatives for font family %S" family))
    candidates))

(defun cat--fontset-signature (role)
  "Return the configuration signature for ROLE's fontset."
  (list
   (cat--font-family-candidates role)
   (cat--font-role-cjk-candidates role)
   (mapcar (lambda (rule)
             (list (car rule)
                   (cat--font-list (cadr rule))
                   (caddr rule)))
           cat-fontset-font-rules)))

(defun cat--set-fontset-candidates (fontset characters fonts &optional add)
  "Set ordered FONTS for CHARACTERS in FONTSET."
  (let ((specs (mapcar (lambda (family)
                         (font-spec :family family
                                    :registry "iso10646-1"))
                       fonts)))
    (cond
     ((null specs)
      (set-fontset-font fontset characters nil))
     (add
      (dolist (spec specs)
        (set-fontset-font fontset characters spec nil add)))
     (t
      ;; Replace with the least preferred candidate, then prepend the rest.
      ;; This preserves the configured order ahead of inherited fallbacks.
      (setq specs (nreverse specs))
      (set-fontset-font fontset characters (pop specs))
      (dolist (spec specs)
        (set-fontset-font fontset characters spec nil 'prepend))))))

(defun cat--configure-role-fontset (fontset signature)
  "Configure FONTSET from a role SIGNATURE."
  (cat--set-fontset-candidates fontset 'ascii (nth 0 signature))
  (dolist (script cat-cjk-scripts)
    (cat--set-fontset-candidates fontset script (nth 1 signature)))
  (pcase-dolist (`(,characters ,fonts ,add) (nth 2 signature))
    (dolist (character (ensure-list characters))
      (cat--set-fontset-candidates fontset character fonts add))))

(defun cat--fontset-for-role (role &optional frame)
  "Return the configured fontset for ROLE, or nil for a non-role."
  (when (cat--font-role-spec role)
    (let* ((fontset (cat--fontset-name role))
           (signature (cat--fontset-signature role)))
      (cond
       ((display-graphic-p frame)
        (let ((created (not (query-fontset fontset))))
          (when created
            (create-fontset-from-fontset-spec fontset))
          (when (or created
                    (not (equal signature
                                (gethash role cat--fontset-signatures))))
            (cat--configure-role-fontset fontset signature)
            (puthash role signature cat--fontset-signatures)))
        fontset)
       ;; `query-fontset' signals before any graphical backend is initialized.
       ((condition-case nil
            (query-fontset fontset)
          (error nil))
        fontset)))))

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
  (if-let* ((face (cat--font-role-face fonts)))
      (if overrides
          (cat--merge-font-attributes (list :inherit face) overrides)
        face)
    (let ((attributes (copy-sequence overrides)))
      (when-let* ((family (cat--face-family fonts)))
        (setq attributes (plist-put attributes :family family)))
      attributes)))

(defun +safe-set-fontset-fonts (fontset characters font-list &optional frame add)
  "Safely set fontset fonts.
If ADD is non-nil, all fonts in FONT-LIST are set with given ADD parameter.
If ADD is nil, use the existing fonts as an ordered replacement."
  (when (display-graphic-p frame)
    (let ((fonts (cat--font-list font-list))
          (families (font-family-list frame))
          available)
      (dolist (font fonts)
        (if (member font families)
            (push font available)
          (warn "Font %s not found" font)))
      (setq available (nreverse available))
      (when available
        (cat--set-fontset-candidates fontset characters available add)
        (dolist (font available)
          (message "Set %s fontset font to %s" characters font))))))


(defun +safe-set-face-fonts (face fonts &optional frame)
  "Safely set FACE family from FONTS or a font role."
  (if-let* ((role-face (cat--font-role-face fonts)))
      (progn
        (set-face-attribute face frame
                            :inherit (list role-face 'fixed-pitch))
        (message "Set %s face font role to %s" face fonts)
        role-face)
    (when-let* ((family (cat--face-family fonts frame)))
      (set-face-attribute face frame :family family :inherit 'fixed-pitch)
      (message "Set %s face font to %s" face family)
      family)))

(defun +safe-buffer-face-set-fonts (fonts)
  "Safely set the current buffer face from FONTS or a font role."
  (when-let* ((spec (cat--resolved-face-spec fonts)))
    (buffer-face-set spec)
    (message "Set buffer %s face to %s" (current-buffer) fonts)
    spec))

(defun cat-setup-fonts (&optional frame)
  "Set fonts on FRAME for Cat Emacs."
  (when (display-graphic-p frame)
    (cat-benchmark 'beg "setup fonts.")
    ;; Configure all role fontsets before their scripts are first displayed.
    (dolist (role cat-font-preset)
      (let* ((name (car role))
             (fontset (cat--fontset-for-role name frame)))
        (cat--configure-font-role-face name frame fontset)))
    (when-let* ((fontset (cat--fontset-for-role 'default frame)))
      (apply #'set-face-attribute 'default frame :font fontset
             (cat--font-role-attributes 'default)))
    (dolist (script cat-cjk-scripts)
      (+safe-set-fontset-fonts
       t script (cat--font-role-cjk-candidates 'default) frame))
    (pcase-dolist (`(,scripts ,fonts . ,args) cat-fontset-font-rules)
      (dolist (script (ensure-list scripts))
        (+safe-set-fontset-fonts t script fonts frame (car args))))
    (run-hook-with-args 'cat-setup-fonts-hook nil frame)
    (cat-benchmark 'end "setup fonts.")))

(add-hook 'cat-theme-refresh-hook #'cat-setup-fonts)
(add-hook 'after-init-hook #'cat-setup-fonts)
(add-hook 'after-make-frame-functions #'cat-setup-fonts)

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
    ;; Enable all ligatures in programming modes
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
  "Mode font state last applied to the current buffer.")

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

(defun cat--font-rule-faces (face)
  "Return faces matched by FACE or its trailing wildcard."
  (let ((name (symbol-name face)))
    (if (and (> (length name) 0)
             (eq (aref name (1- (length name))) ?*))
        (let ((prefix (substring name 0 -1)))
          (sort (seq-filter
                 (lambda (candidate)
                   (string-prefix-p prefix (symbol-name candidate)))
                 (face-list))
                (lambda (left right)
                  (string< (symbol-name left) (symbol-name right)))))
      (and (facep face) (list face)))))

(defun cat--apply-mode-font-rule (rule)
  "Apply a mode font RULE to the current buffer."
  (cat--clear-mode-font)
  (when-let* ((font (plist-get rule :font)))
    (when-let* ((spec (+safe-buffer-face-set-fonts font)))
      (setq cat--mode-buffer-face spec)))
  (pcase-dolist (`(,face ,fonts . ,attributes) (plist-get rule :faces))
    (dolist (matched-face (cat--font-rule-faces face))
      (when-let* ((spec (cat--resolved-face-spec fonts attributes)))
        (push (face-remap-add-relative matched-face spec)
              cat--mode-face-remap-cookies))))
  (when-let* ((rescale (plist-get rule :rescale)))
    (setq cat--mode-font-rescale-state
          (list (local-variable-p 'face-font-rescale-alist)
                face-font-rescale-alist))
    (setq-local face-font-rescale-alist rescale)))

(defun cat-setup-mode-font (&optional force)
  "Set fonts according to the current major mode.
With FORCE, reapply the configured fonts.  Respect a `buffer-face-mode'
owned by other configuration."
  (unless (and (bound-and-true-p buffer-face-mode)
               (not (equal buffer-face-mode-face cat--mode-buffer-face)))
    (let* ((rule (seq-find #'cat--mode-font-rule-matches-p
                           cat-mode-font-rules))
           (state (list major-mode rule)))
      (when (or force (not (equal state cat--mode-font-state)))
        (if rule
            (cat--apply-mode-font-rule rule)
          (cat--clear-mode-font))
        (setq cat--mode-font-state state)))))

(defun cat--refresh-mode-fonts (&rest _)
  "Reapply Cat's buffer-local font rules in every live buffer."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when cat--mode-font-state
        (cat-setup-mode-font t)))))

(add-hook 'cat-setup-fonts-hook #'cat--refresh-mode-fonts)
(add-hook 'window-configuration-change-hook 'cat-setup-mode-font)
(add-hook 'after-change-major-mode-hook 'cat-setup-mode-font)
(add-hook 'after-revert-hook 'cat-setup-mode-font)

(when (and after-init-time (display-graphic-p))
  (cat-setup-fonts))
