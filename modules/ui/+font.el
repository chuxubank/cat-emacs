;; -*- lexical-binding: t; -*-

(defgroup cat-font nil
  "Font settings for Cat Emacs."
  :group 'cat)

(defconst cat-font--role-override-properties
  '(:stack :fonts :height :weight :slant :width)
  "Properties accepted in named and buffer-local role overrides.")

(defvar cat--font-configuration-version 0
  "Generation number of the current font configuration.")

(defun cat-font--valid-face-attribute-value-p (property value)
  "Return non-nil when VALUE is valid for face PROPERTY."
  (let ((table (pcase property
                 (:weight font-weight-table)
                 (:slant font-slant-table)
                 (:width font-width-table))))
    (and (symbolp value)
         (seq-some (lambda (entry) (seq-contains-p entry value)) table))))

(defun cat-font--validate-role-overrides
    (owner overrides &optional preset stacks)
  "Validate OWNER's role OVERRIDES against PRESET and STACKS."
  (let ((preset (or preset cat-font-preset))
        (stacks (or stacks cat-font-stacks))
        roles)
    (unless (proper-list-p overrides)
      (error "Font role overrides for %S must be a list" owner))
    (dolist (entry overrides)
      (unless (and (consp entry)
                   (symbolp (car entry))
                   (proper-list-p (cdr entry))
                   (zerop (% (length (cdr entry)) 2)))
        (error "Invalid font role override for %S: %S" owner entry))
      (let ((role (car entry))
            (properties (cdr entry)))
        (unless (assq role preset)
          (error "Unknown font role %S in preset %S" role owner))
        (when (memq role roles)
          (error "Duplicate font role %S in preset %S" role owner))
        (push role roles)
        (cl-loop for (property value) on properties by #'cddr
                 unless (memq property cat-font--role-override-properties)
                 do (error "Unsupported font role property %S in preset %S"
                           property owner)
                 when (and (eq property :stack)
                           (not (assq value stacks)))
                 do (error "Unknown font stack %S in preset %S" value owner)
                 when (and (eq property :fonts)
                           (not (and (proper-list-p value)
                                     (seq-every-p #'stringp value))))
                 do (error ":fonts must be a list of strings in preset %S"
                           owner)
                 when (and (eq property :height)
                           (not (and (numberp value) (> value 0))))
                 do (error ":height must be positive in preset %S" owner)
                 when (and (memq property '(:weight :slant :width))
                           (not (cat-font--valid-face-attribute-value-p
                                 property value)))
                 do (error "Invalid %S value %S in preset %S"
                           property value owner)))))
  overrides)

(defun cat-font--validate-presets (presets &optional preset stacks)
  "Validate named PRESETS against the base PRESET and STACKS."
  (unless (proper-list-p presets)
    (error "Font presets must be a list"))
  (let (names)
    (dolist (entry presets)
      (unless (and (consp entry) (symbolp (car entry)))
        (error "Invalid named font preset: %S" entry))
      (when (memq (car entry) names)
        (error "Duplicate font preset %S" (car entry)))
      (push (car entry) names)
      (cat-font--validate-role-overrides
       (car entry) (cdr entry) preset stacks)))
  presets)

(defun cat-font--configuration-changed ()
  "Invalidate generated font state and refresh visible buffers."
  (cl-incf cat--font-configuration-version)
  (when (boundp 'cat--fontset-signatures)
    (clrhash cat--fontset-signatures))
  (when (boundp 'cat--font-role-face-signatures)
    (clrhash cat--font-role-face-signatures))
  (when (fboundp 'cat-setup-fonts)
    (if (display-graphic-p)
        (cat-setup-fonts)
      (when (fboundp 'cat--refresh-mode-fonts)
        (cat--refresh-mode-fonts)))))

(defun cat-font--rule-roles (rule)
  "Return semantic font roles referenced by RULE."
  (delq nil
        (cons (when (symbolp (plist-get rule :font))
                (plist-get rule :font))
              (mapcar (lambda (face-rule)
                        (when (symbolp (cadr face-rule))
                          (cadr face-rule)))
                      (plist-get rule :faces)))))

(defun cat-font-validate-rule (owner rule &optional preset)
  "Validate roles and stepped faces in OWNER's font RULE against PRESET."
  (let ((preset (or preset cat-font-preset)))
    (dolist (role (cat-font--rule-roles rule))
      (unless (assq role preset)
        (error "Unknown font role %S in font rule for %S" role owner)))
    (dolist (face-rule (plist-get rule :faces))
      (let ((attributes (cddr face-rule)))
        (unless (zerop (% (length attributes) 2))
          (error "Invalid face attributes in font rule for %S" owner))
        (dolist (property '(:height-step :weight-step))
          (when (and (plist-member attributes property)
                     (not (numberp (plist-get attributes property))))
            (error "%S must be numeric in font rule for %S"
                   property owner))))))
  rule)

(defun cat-font--set-preset (symbol value)
  "Set SYMBOL to VALUE after validating registered font rules."
  (dolist (entry cat-font-rule-alist)
    (cat-font-validate-rule (car entry) (cdr entry) value))
  (when (boundp 'cat-mode-font-rules)
    (dolist (rule cat-mode-font-rules)
      (cat-font-validate-rule 'cat-mode-font-rules rule value)))
  (when (boundp 'cat-font-presets)
    (cat-font--validate-presets cat-font-presets value))
  (set-default symbol value)
  (cat-font--configuration-changed))

(defun cat-font--set-presets (symbol value)
  "Set named font preset SYMBOL to VALUE after validation."
  (cat-font--validate-presets value)
  (set-default symbol value)
  (cat-font--configuration-changed))

(defun cat-font--set-buffer-preset (symbol value)
  "Set the default buffer-local preset SYMBOL to VALUE."
  (unless (or (null value) (assq value cat-font-presets))
    (error "Unknown font preset %S" value))
  (set-default symbol value)
  (cat-font--configuration-changed))

(defun cat-font--set-buffer-role-overrides (symbol value)
  "Set default buffer-local role override SYMBOL to VALUE."
  (cat-font--validate-role-overrides symbol value)
  (set-default symbol value)
  (cat-font--configuration-changed))

(defun cat-font--set-mode-rules (symbol value)
  "Set SYMBOL to mode font rules VALUE after validating their roles."
  (dolist (rule value)
    (cat-font-validate-rule symbol rule))
  (set-default symbol value))

(defun cat-font--set-stacks (symbol value)
  "Set SYMBOL to font stacks VALUE and refresh configured fonts."
  (when (boundp 'cat-font-preset)
    (dolist (entry cat-font-preset)
      (when-let* ((stack (plist-get (cdr entry) :stack)))
        (unless (assq stack value)
          (error "Unknown font stack %S for role %S" stack (car entry)))))
    (when (boundp 'cat-font-presets)
      (cat-font--validate-presets cat-font-presets cat-font-preset value)))
  (set-default symbol value)
  (cat-font--configuration-changed))

(defcustom cat-font-stacks
  '((fallback
     :symbol ("Apple Symbols" "Symbola")
     :mathematical ("STIX Two Math"
                    "DejaVu Math TeX Gyre"
                    "Noto Sans Math")
     :emoji ("Apple Color Emoji"))
    (sans-serif
     :extends fallback
     :ascii ("Inter" "Avenir Next" "DejaVu Sans")
     :cjk ("LXGW Neo XiHei" "Source Han Sans SC" "PingFang SC" "Noto Sans CJK SC"
           "Hiragino Sans GB" "Microsoft YaHei"))
    (serif
     :extends fallback
     :ascii ("EB Garamond" "Athelas" "Iowan Old Style" "Baskerville"
             "Roboto Serif" "DejaVu Serif" "Georgia")
     :cjk ("Zhuque Fangsong (technical preview)" "LXGW Neo ZhiSong"
           "Source Han Serif SC VF" "Songti SC" "STFangsong"
           "LXGW WenKai" "Noto Serif CJK SC"))
    (slab-serif
     :extends serif
     :ascii ("Roboto Slab" "American Typewriter"))
    (cursive
     :extends serif
     :ascii ("Snell Roundhand" "Apple Chancery" "Zapfino")
     :cjk ("Xingkai SC" "Kaiti SC" "STKaiti"))
    (quasi-proportional
     :extends serif
     :ascii ("Iosevka Etoile" "Iosevka Aile")
     :cjk ("LXGW WenKai TC" "LXGW WenKai"
           "Source Han Serif SC VF" "Noto Serif CJK SC"))
    (monospace-narrow
     :extends fallback
     :ascii ("Iosevka" "Iosevka Term")
     :symbol ("Iosevka")
     :cjk ("LXGW WenKai Mono" "Sarasa Mono SC"))
    (monospace-align
     :extends monospace-narrow
     :ascii ("Maple Mono")
     :cjk ("Maple Mono CN"))
    (monospace-code
     :extends monospace-align
     :ascii ("Source Code Pro"))
    (monospace-sans-serif
     :extends monospace-narrow
     :ascii ("Roboto Mono" "DejaVu Sans Mono")))
  "Physical font candidates grouped into reusable stacks.
Each entry has the form (STACK :CATEGORY FONTS...).  Categories include
:ascii, :cjk, :symbol, :mathematical, and :emoji.  A stack can inherit
missing properties from another stack with :extends.  Font categories defined
by both stacks are combined with the child candidates first and duplicates
removed."
  :type 'sexp
  :group 'cat-font
  :set #'cat-font--set-stacks)

(defcustom cat-font-preset
  `((default :stack monospace-narrow
             :height ,(if IS-MAC 160 140))
    (title :stack serif :weight heavy :height 2.0)
    (heading :stack serif :height 1.5)
    (body :stack monospace-sans-serif)
    (documentation :extends body)
    (prose :stack quasi-proportional)
    (decorative :stack cursive)
    (ui :stack sans-serif)
    (metadata-label :stack monospace-sans-serif)
    (metadata-value :stack monospace-narrow)
    (mono :stack monospace-sans-serif)
    (code :stack monospace-code)
    (table :stack monospace-align)
    (code-jvm :extends code :fonts ("JetBrains Mono"))
    (code-python :extends code :fonts ("Cascadia Code"))
    (code-diagram :extends code :fonts ("Fira Code"))
    (code-apple :extends code :fonts ("SF Mono"))
    (code-config :extends code :fonts ("IBM Plex Mono"))
    (terminal :extends mono :fonts ("Menlo")))
  "Typography preset organized by semantic role.
Each role has the form (ROLE :stack STACK &rest ATTRIBUTES).  STACK
names an entry in `cat-font-stacks'.  The
`default' role uses an absolute face height in tenths of a point;
content roles use heights relative to it.  A role can use :extends and
:fonts to prepend concrete families to its inherited stack."
  :type 'sexp
  :group 'cat-font
  :set #'cat-font--set-preset)

(defcustom cat-font-presets
  '((modern
     (title :stack sans-serif :fonts ("Inter Display") :weight bold)
     (heading :stack sans-serif :fonts ("Inter") :weight semi-bold)
     (body :stack sans-serif :fonts ("Inter"))
     (prose :stack sans-serif :fonts ("Inter"))
     (decorative :stack sans-serif :fonts ("Inter") :slant italic)
     (ui :stack sans-serif :fonts ("Inter"))
     (metadata-label :stack sans-serif :fonts ("Inter")
                     :weight semi-bold))
    (classical
     (title :stack serif :fonts ("EB Garamond"))
     (heading :stack serif :fonts ("Athelas"))
     (body :stack serif :fonts ("Iowan Old Style"))
     (prose :stack serif :fonts ("Iowan Old Style"))
     (decorative :stack cursive)
     (metadata-label :stack slab-serif))
    (technical
     (title :stack sans-serif :fonts ("DIN Condensed") :weight bold)
     (heading :stack sans-serif :fonts ("Avenir Next") :weight semi-bold)
     (body :stack serif :fonts ("STIX Two Text"))
     (prose :stack serif :fonts ("STIX Two Text"))
     (decorative :stack slab-serif :fonts ("Roboto Slab"))
     (ui :stack sans-serif :fonts ("Avenir Next"))
     (metadata-label :stack sans-serif :fonts ("Avenir Next")
                     :weight semi-bold)
     (mono :stack monospace-narrow :fonts ("SF Mono"))
     (code :stack monospace-code :fonts ("SF Mono"))))
  "Named role overrides applied on top of `cat-font-preset'.
Each entry has the form (NAME (ROLE PROPERTY VALUE ...) ...).  Supported
properties are :stack, :fonts, :height, :weight, :slant, and :width."
  :type 'sexp
  :group 'cat-font
  :set #'cat-font--set-presets)

(defcustom cat-font-buffer-preset nil
  "Named font preset selected for the current buffer.
Nil uses the base `cat-font-preset'."
  :type '(choice (const :tag "Base preset" nil) symbol)
  :group 'cat-font
  :set #'cat-font--set-buffer-preset)
(make-variable-buffer-local 'cat-font-buffer-preset)

(defcustom cat-font-buffer-role-overrides nil
  "Role overrides applied only to the current buffer.
The value uses the same role override format as `cat-font-presets'."
  :type 'sexp
  :group 'cat-font
  :set #'cat-font--set-buffer-role-overrides)
(make-variable-buffer-local 'cat-font-buffer-role-overrides)

(defun cat-font--safe-buffer-preset-p (value)
  "Return non-nil when VALUE names a configured font preset."
  (or (null value)
      (and (symbolp value) (assq value cat-font-presets))))

(defun cat-font--safe-buffer-role-overrides-p (value)
  "Return non-nil when VALUE is a valid local role override list."
  (condition-case nil
      (progn
        (cat-font--validate-role-overrides 'file-local value)
        t)
    (error nil)))

(put 'cat-font-buffer-preset 'safe-local-variable
     #'cat-font--safe-buffer-preset-p)
(put 'cat-font-buffer-role-overrides 'safe-local-variable
     #'cat-font--safe-buffer-role-overrides-p)

(defcustom cat-font-script-rules
  '(((han kana hangul bopomofo cjk-misc) cjk)
    (symbol symbol)
    (mathematical mathematical)
    (emoji emoji))
  "Map fontset character targets to stack categories.
Each rule has the form (CHARACTERS CATEGORY &optional ADD).  CHARACTERS
can be a script symbol or a list of script symbols.  CATEGORY names a
font category configured in `cat-font-stacks'."
  :type 'sexp
  :group 'cat-font)

(defcustom cat-mode-font-rules
  `((:modes (nxml-mode sgml-mode toml-ts-mode conf-mode)
            :font code-config)
    (:modes (prog-mode)
            :font code)
    (:modes (text-mode)
            :font prose))
  "Fallback rules for buffer-local font selection.
Matching module rules are layered in declaration order, while the first one
providing :font or :rescale owns that setting.  The first matching rule here is
used only when no module rule matches.  Each rule is a plist.  Supported keys are:

:modes       A mode or list of modes matched with `derived-mode-p'.
:buffer-name A regexp matched against `buffer-name'.
:font        Font role, concrete family, or ordered family list.
:faces       Face rules in the form (FACE FONTS-OR-ROLE &rest ATTRIBUTES).
             A FACE ending in * matches every face with that prefix, in
             version-aware name order.  :height-step adds a numeric delta and
             :weight-step moves through standard font weights for each match
             after the first.  Role attributes provide their starting values.
:rescale     Buffer-local `face-font-rescale-alist' value."
  :type 'sexp
  :group 'cat-font
  :set #'cat-font--set-mode-rules)

(setq use-default-font-for-symbols (not STIPPLE-COMPATIBLE-P))

(dolist (entry cat-font-rule-alist)
  (cat-font-validate-rule (car entry) (cdr entry)))
(dolist (rule cat-mode-font-rules)
  (cat-font-validate-rule 'cat-mode-font-rules rule))

(defvar cat-setup-fonts-hook nil
  "Hook runs after setup fonts.")

(defun cat--merge-font-attributes (base overrides)
  "Return face attributes from BASE with OVERRIDES applied."
  (let ((attributes (copy-sequence base)))
    (while overrides
      (setq attributes
            (plist-put attributes (pop overrides) (pop overrides))))
    attributes))

(defvar-local cat--effective-font-preset-cache nil
  "Cached effective font preset for the current buffer.")

(defun cat--merge-font-role-overrides (preset overrides)
  "Return PRESET with role OVERRIDES merged into it."
  (let ((result (copy-tree preset)))
    (dolist (entry overrides)
      (let ((role (assq (car entry) result)))
        (setcdr role
                (cat--merge-font-attributes (cdr role) (cdr entry)))))
    result))

(defun cat--effective-font-preset ()
  "Return the effective role preset for the current buffer."
  (let ((key (list cat--font-configuration-version
                   cat-font-buffer-preset
                   cat-font-buffer-role-overrides)))
    (if (equal key (car cat--effective-font-preset-cache))
        (cadr cat--effective-font-preset-cache)
      (let* ((named
              (when cat-font-buffer-preset
                (or (cdr (assq cat-font-buffer-preset cat-font-presets))
                    (error "Unknown font preset %S"
                           cat-font-buffer-preset))))
             (_ (cat-font--validate-role-overrides
                 'buffer-local cat-font-buffer-role-overrides))
             (preset (cat--merge-font-role-overrides
                      cat-font-preset named))
             (preset (cat--merge-font-role-overrides
                      preset cat-font-buffer-role-overrides)))
        (setq cat--effective-font-preset-cache
              (list (copy-tree key) preset))
        preset))))

(defun cat--font-role-spec-from-preset (role preset &optional seen)
  "Return ROLE's inherited specification from PRESET."
  (when (symbolp role)
    (when (memq role seen)
      (error "Circular font role inheritance involving %S" role))
    (when-let* ((spec (alist-get role preset)))
      (let ((parent (plist-get spec :extends)))
        (if parent
            (cat--merge-font-attributes
             (or (cat--font-role-spec-from-preset
                  parent preset (cons role seen))
                 (error "Unknown parent font role %S for %S" parent role))
             spec)
          spec)))))

(defun cat--font-role-spec (role)
  "Return ROLE's effective font specification for the current buffer."
  (cat--font-role-spec-from-preset role (cat--effective-font-preset)))

(defun cat--base-font-role-spec (role)
  "Return ROLE's specification from the base font preset."
  (cat--font-role-spec-from-preset role cat-font-preset))

(defun cat--merge-font-stack-specs (parent child)
  "Merge PARENT and CHILD stack specs with child candidates first."
  (let ((result (copy-sequence parent)))
    (cl-loop for (property value) on child by #'cddr
             do (setq result
                      (plist-put
                       result property
                       (if (memq property
                                 '(:ascii :cjk :symbol :mathematical :emoji))
                           (delete-dups
                            (append value (plist-get result property) nil))
                         value))))
    result))

(defun cat--font-stack-spec (stack)
  "Return the inherited specification for font STACK."
  (when (symbolp stack)
    (let* ((spec (alist-get stack cat-font-stacks))
           (parent (plist-get spec :extends)))
      (unless spec
        (error "Unknown font stack: %S" stack))
      (if parent
          (cat--merge-font-stack-specs (cat--font-stack-spec parent) spec)
        spec))))

(defun cat--font-role-candidates (role script)
  "Return ordered font candidates for ROLE and SCRIPT category."
  (let* ((role-spec (cat--font-role-spec role))
         (stack (plist-get role-spec :stack))
         (stack-spec (cat--font-stack-spec stack))
         (property (intern (format ":%s" script)))
         (fonts (copy-sequence (plist-get stack-spec property))))
    (when (eq script 'ascii)
      (setq fonts (append (plist-get role-spec :fonts) fonts)))
    (unless (and fonts (seq-every-p #'stringp fonts))
      (error "No %s fonts configured for role %S" script role))
    (delete-dups fonts)))

(defun cat--font-list (fonts)
  "Return concrete FONTS or a font role as an ordered list."
  (cond
   ((cat--font-role-spec fonts)
    (cat--font-role-candidates fonts 'ascii))
   ((null fonts) nil)
   ((stringp fonts) (list fonts))
   ((and (listp fonts) (seq-every-p #'stringp fonts)) fonts)
   (t (error "Invalid font value: %S" fonts))))

(defun cat--font-role-attributes (role)
  "Return face attributes associated with font ROLE."
  (cl-loop for (attribute value) on (cat--font-role-spec role) by #'cddr
           unless (memq attribute '(:stack :fonts :extends))
           append (list attribute value)))

(defun cat--font-data-hash (data)
  "Return a short stable hash for font configuration DATA."
  (substring (secure-hash 'sha1 (prin1-to-string data)) 0 12))

(defun cat--font-role-face-name (role spec)
  "Return the face name for ROLE with effective SPEC."
  (intern
   (if (equal spec (cat--base-font-role-spec role))
       (format "cat-font-role-%s" role)
     (format "cat-font-role-%s-%s" role (cat--font-data-hash spec)))))

(defvar cat--font-role-face-signatures (make-hash-table :test 'equal)
  "Last configured signature for each Cat role face and frame.")

(defun cat--font-role-face (role &optional frame)
  "Return ROLE's face for the current buffer, creating it on FRAME."
  (when-let* ((spec (cat--font-role-spec role)))
    (let* ((face (cat--font-role-face-name role spec))
           (frame (or frame (selected-frame))))
      (unless (facep face)
        (make-empty-face face))
      (let* ((fontset (and (fboundp 'cat--fontset-for-role)
                           (cat--fontset-for-role role frame)))
             (signature (list spec fontset))
             (key (cons face frame)))
        (unless (equal signature
                       (gethash key cat--font-role-face-signatures))
          (dolist (attribute (mapcar #'car face-attribute-name-alist))
            (set-face-attribute face frame attribute 'unspecified))
          ;; `:font' resolves Latin; `:fontset' restores script mappings.
          (apply #'set-face-attribute face frame
                 (append (when fontset
                           (list :font fontset :fontset fontset))
                         (cat--font-role-attributes role)))
          (puthash key signature cat--font-role-face-signatures)))
      face)))

(defvar cat--fontset-signatures (make-hash-table :test 'equal)
  "Last configured signature for each Cat fontset name.")

(defvar cat--default-fontset-signature nil
  "Last Cat configuration applied to the default fontset.")

(defvar cat--nerd-icons-fontset-entry-cache nil
  "Fontset entries discovered from `nerd-icons-set-font'.")

(defun cat--nerd-icons-fontset-entries ()
  "Return fontset entries maintained by Nerd Icons."
  (or cat--nerd-icons-fontset-entry-cache
      (progn
        (require 'nerd-icons)
        ;; Keep Nerd Icons as the source of truth for its evolving PUA ranges.
        (let (entries)
          (cl-letf (((symbol-function 'set-fontset-font)
                     (lambda (_fontset characters _font-spec
                                       &optional _frame add)
                       (push (cons characters add) entries))))
            (nerd-icons-set-font))
          (unless entries
            (error "Nerd Icons provided no fontset entries"))
          (setq cat--nerd-icons-fontset-entry-cache
                (nreverse entries))))))

(defun cat--font-rules (role)
  "Return ROLE's configured script and generated Nerd Icons font rules."
  (require 'nerd-icons)
  (append (mapcar (lambda (rule)
                    (list (car rule)
                          (cat--font-role-candidates role (cadr rule))
                          (caddr rule)))
                  cat-font-script-rules)
          (mapcar (lambda (entry)
                    (list (list (car entry))
                          (list nerd-icons-font-family)
                          (cdr entry)))
                  (cat--nerd-icons-fontset-entries))))

(defun cat--fontset-name (role signature)
  "Return the fontset name for ROLE and resolved SIGNATURE."
  (format "-*-cat-*-*-*-*-*-*-*-*-*-*-fontset-cat_%s_%s"
          (replace-regexp-in-string "-" "_" (symbol-name role))
          (cat--font-data-hash signature)))

(defun cat--fontset-signature (role &optional frame)
  "Return ROLE's available fontset configuration on FRAME."
  (let* ((graphical (display-graphic-p frame))
         (families (and graphical (font-family-list frame)))
         (available
          (lambda (fonts)
            (if graphical
                (seq-filter (lambda (font)
                              (member font families))
                            fonts)
              fonts))))
    (list
     (funcall available (cat--font-role-candidates role 'ascii))
     (mapcar (lambda (rule)
               (list (nth 0 rule)
                     (funcall available (nth 1 rule))
                     (nth 2 rule)))
             (cat--font-rules role)))))

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
  (pcase-dolist (`(,characters ,fonts ,add) (nth 1 signature))
    (dolist (character (ensure-list characters))
      (cat--set-fontset-candidates fontset character fonts add))))

(defun cat--fontset-for-role (role &optional frame)
  "Return the configured fontset for ROLE, or nil for a non-role."
  (when (cat--font-role-spec role)
    (let* ((signature (cat--fontset-signature role frame))
           (fontset (cat--fontset-name role signature)))
      (cond
       ((display-graphic-p frame)
        (let ((created (not (query-fontset fontset))))
          (when created
            (create-fontset-from-fontset-spec fontset))
          (when (or created
                    (not (equal signature
                                (gethash fontset cat--fontset-signatures))))
            (cat--configure-role-fontset fontset signature)
            (puthash fontset signature cat--fontset-signatures)))
        fontset)
       ;; `query-fontset' signals before any graphical backend is initialized.
       ((condition-case nil
            (query-fontset fontset)
          (error nil))
        fontset)))))

(defun cat--first-existing-font (fonts &optional frame)
  "Return the first font from FONTS available on FRAME."
  (when (display-graphic-p frame)
    (let* ((candidates (cat--font-list fonts))
           (families (font-family-list frame))
           (font (seq-find (lambda (candidate)
                             (member candidate families))
                           candidates)))
      (unless font
        (warn "No candidate font found: %s"
              (string-join candidates ", ")))
      font)))

(defun cat--face-family (fonts &optional frame)
  "Return the first installed family for FONTS on FRAME."
  (cat--first-existing-font fonts frame))

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
  (let ((cat-font-buffer-preset nil)
        (cat-font-buffer-role-overrides nil))
    (if-let* ((role-face (cat--font-role-face fonts frame)))
        (progn
          (set-face-attribute face frame
                              :inherit (list role-face 'fixed-pitch))
          (message "Set %s face font role to %s" face fonts)
          role-face)
      (when-let* ((family (cat--face-family fonts frame)))
        (set-face-attribute face frame :family family :inherit 'fixed-pitch)
        (message "Set %s face font to %s" face family)
        family))))

(defun +safe-buffer-face-set-fonts (fonts)
  "Set the current graphical buffer face from FONTS or a font role."
  (when (display-graphic-p)
    (when-let* ((spec (cat--resolved-face-spec fonts)))
      (buffer-face-set spec)
      (message "Set buffer %s face to %s" (current-buffer) fonts)
      spec)))

(defun cat--configure-default-fontset (signature frame)
  "Apply default fontset SIGNATURE once using graphical FRAME."
  (unless (equal signature cat--default-fontset-signature)
    (pcase-dolist (`(,scripts ,fonts . ,args) (nth 1 signature))
      (dolist (script (ensure-list scripts))
        (+safe-set-fontset-fonts t script fonts frame (car args))))
    (setq cat--default-fontset-signature signature)))

(defun cat-setup-fonts (&optional frame)
  "Set fonts on FRAME for Cat Emacs."
  (when (display-graphic-p frame)
    (let ((cat-font-buffer-preset nil)
          (cat-font-buffer-role-overrides nil))
      (cat-benchmark 'beg "setup fonts.")
      ;; Configure the base role fontsets before scripts are first displayed.
      (dolist (role cat-font-preset)
        (cat--font-role-face (car role) frame))
      (when-let* ((fontset (cat--fontset-for-role 'default frame)))
        (apply #'set-face-attribute 'default frame
               :font fontset :fontset fontset
               (cat--font-role-attributes 'default)))
      (cat--configure-default-fontset
       (cat--fontset-signature 'default frame) frame)
      (run-hook-with-args 'cat-setup-fonts-hook nil frame)
      (cat-benchmark 'end "setup fonts."))))

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

(defvar cat--nerd-icons-fontset-configurations nil
  "Nerd Icons frame, fontset, and family triples already configured.")

(defun cat--setup-nerd-icons-font (&optional _font-family frame)
  "Configure Nerd Icons once for graphical FRAME's active fontset."
  (let* ((frame (or frame (selected-frame)))
         (fontset (and (display-graphic-p frame)
                       (frame-parameter frame 'font))))
    (when fontset
      (require 'nerd-icons)
      (let ((configuration (list frame fontset nerd-icons-font-family)))
        (unless (member configuration
                        cat--nerd-icons-fontset-configurations)
          (with-selected-frame frame
            (nerd-icons-set-font nil frame))
          (push configuration
                cat--nerd-icons-fontset-configurations))))))

(add-hook 'cat-setup-fonts-hook #'cat--setup-nerd-icons-font)

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
   ("Apple Chancery" . 0.9)
   ("Noto Serif .+" . 0.9)
   ("Source Han .+" . 0.9)
   ("Zhuque Fangsong .+" . 0.9)
   ("-cdac$" . 1.3)))

(defun cat--font-rule-matches-mode-p (rule mode)
  "Return non-nil when font RULE applies to major MODE."
  (when-let* ((modes (plist-get rule :modes)))
    (or (memq mode (ensure-list modes))
        (provided-mode-derived-p mode modes))))

(defun cat--mode-font-rule-matches-p (rule)
  "Return non-nil when RULE applies to the current buffer."
  (or (cat--font-rule-matches-mode-p rule major-mode)
      (when-let* ((regexp (plist-get rule :buffer-name)))
        (string-match-p regexp (buffer-name)))))

(defun cat-font-for-mode (mode)
  "Return the font selected for major MODE by Cat's font rules."
  (let ((rules
         (seq-filter
          (lambda (rule) (cat--font-rule-matches-mode-p rule mode))
          (mapcar #'cdr cat-font-rule-alist))))
    (unless rules
      (when-let* ((fallback
                   (seq-find (lambda (rule)
                               (cat--font-rule-matches-mode-p rule mode))
                             cat-mode-font-rules)))
        (setq rules (list fallback))))
    (when-let* ((rule (seq-find (lambda (candidate)
                                  (plist-get candidate :font))
                                rules)))
      (plist-get rule :font))))

(defun cat-font-apply-mode-to-region (mode start end)
  "Prepend the font selected for MODE to the region from START to END."
  (when-let* ((font (cat-font-for-mode mode))
              (spec (cat--resolved-face-spec font)))
    (with-silent-modifications
      (add-face-text-property start end spec))
    font))

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

(defun cat--font-step-weight (weight step index)
  "Move WEIGHT by STEP for wildcard match INDEX."
  (let* ((entry (seq-find (lambda (candidate)
                            (seq-contains-p candidate weight))
                          font-weight-table))
         (position (and entry (seq-position font-weight-table entry))))
    (unless position
      (error "Cannot step unknown font weight %S" weight))
    (aref (aref font-weight-table
                (max 0
                     (min (1- (length font-weight-table))
                          (floor (+ position (* step index) 0.5)))))
          1)))

(defun cat--font-stepped-attributes (fonts attributes index)
  "Resolve stepped face ATTRIBUTES for FONTS at wildcard match INDEX."
  (let* ((role-spec (cat--font-role-spec fonts))
         (height-step (plist-get attributes :height-step))
         (weight-step (plist-get attributes :weight-step))
         (attributes
          (cl-loop for (property value) on attributes by #'cddr
                   unless (memq property '(:height-step :weight-step))
                   append (list property value))))
    (when height-step
      (let* ((role-height (plist-get role-spec :height))
             (base (or (plist-get attributes :height)
                       role-height
                       1.0))
             (height (+ base (* height-step index))))
        (unless (and (numberp base)
                     (> height 0)
                     (or (floatp base) (integerp height)))
          (error "Invalid stepped face height %S from base %S" height base))
        ;; Relative heights inherited from the role already participate in
        ;; face remapping, so apply only the ratio needed to reach HEIGHT.
        (setq attributes
              (plist-put attributes :height
                         (if (and (floatp role-height) (floatp height))
                             (/ height role-height)
                           height)))))
    (when weight-step
      (let ((base (or (plist-get attributes :weight)
                      (plist-get role-spec :weight)
                      'normal)))
        (setq attributes
              (plist-put attributes :weight
                         (cat--font-step-weight base weight-step index)))))
    attributes))

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
                  (string-version-lessp (symbol-name left)
                                        (symbol-name right)))))
      (and (facep face) (list face)))))

(defun cat--matching-mode-font-rules ()
  "Return the module font rules for the current buffer.
Use the first matching fallback rule only when no module rule matches."
  (let ((rules (seq-filter #'cat--mode-font-rule-matches-p
                           (mapcar #'cdr cat-font-rule-alist))))
    (or rules
        (when-let* ((fallback
                     (seq-find #'cat--mode-font-rule-matches-p
                               cat-mode-font-rules)))
          (list fallback)))))

(defun cat--apply-mode-font-rules (rules)
  "Apply matching mode font RULES to the current buffer."
  (cat--clear-mode-font)
  (when-let* ((rule (seq-find (lambda (candidate)
                                (plist-get candidate :font))
                              rules))
              (font (plist-get rule :font))
              (spec (+safe-buffer-face-set-fonts font)))
    (setq cat--mode-buffer-face spec))
  (dolist (rule rules)
    (pcase-dolist (`(,face ,fonts . ,attributes) (plist-get rule :faces))
      (cl-loop for matched-face in (cat--font-rule-faces face)
               for index from 0
               for stepped = (cat--font-stepped-attributes
                              fonts attributes index)
               for spec = (cat--resolved-face-spec fonts stepped)
               when spec
               do (push (face-remap-add-relative matched-face spec)
                        cat--mode-face-remap-cookies))))
  (when-let* ((rule (seq-find (lambda (candidate)
                                (plist-get candidate :rescale))
                              rules))
              (rescale (plist-get rule :rescale)))
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
    (let* ((rules (cat--matching-mode-font-rules))
           (state (list major-mode rules
                        cat--font-configuration-version
                        cat-font-buffer-preset
                        (copy-tree cat-font-buffer-role-overrides))))
      (when (or force (not (equal state cat--mode-font-state)))
        (if rules
            (cat--apply-mode-font-rules rules)
          (cat--clear-mode-font))
        (setq cat--mode-font-state state)))))

(defun cat--setup-window-fonts (window-or-frame)
  "Apply mode fonts to buffers visible in WINDOW-OR-FRAME."
  (dolist (window (if (windowp window-or-frame)
                      (list window-or-frame)
                    (window-list window-or-frame 'no-minibuf)))
    (with-current-buffer (window-buffer window)
      (cat-setup-mode-font))))

(defun cat--setup-visible-mode-font (&rest _)
  "Apply mode fonts when the current buffer is visible."
  (when (get-buffer-window (current-buffer) 'visible)
    (cat-setup-mode-font)))

(defun cat--refresh-mode-fonts (&rest _)
  "Reapply Cat's mode font rules to visible buffers."
  (let (buffers)
    (walk-windows (lambda (window)
                    (cl-pushnew (window-buffer window) buffers))
                  'no-minibuf 'visible)
    (dolist (buffer buffers)
      (with-current-buffer buffer
        (cat-setup-mode-font t)))))

(defun cat-font-refresh-buffer ()
  "Refresh font settings and native fontification in the current buffer."
  (interactive)
  (setq cat--effective-font-preset-cache nil
        cat--mode-font-state nil)
  (when (get-buffer-window (current-buffer) 'visible)
    (cat-setup-mode-font t))
  (when (bound-and-true-p font-lock-mode)
    (font-lock-flush)
    (when (get-buffer-window (current-buffer) 'visible)
      (font-lock-ensure))))

(defun cat-font-select-buffer-preset (selection)
  "Select a named font preset for the current buffer.
SELECTION may also restore the default value or select the base preset."
  (interactive
   (let* ((choices
           (append '("<default>" "<base>")
                   (mapcar (lambda (entry) (symbol-name (car entry)))
                           cat-font-presets)))
          (current (if (local-variable-p 'cat-font-buffer-preset)
                       (if cat-font-buffer-preset
                           (symbol-name cat-font-buffer-preset)
                         "<base>")
                     "<default>")))
     (list (completing-read "Buffer font preset: " choices nil t
                            nil nil current))))
  (pcase selection
    ("<default>" (kill-local-variable 'cat-font-buffer-preset))
    ("<base>" (setq-local cat-font-buffer-preset nil))
    ((pred symbolp)
     (unless (assq selection cat-font-presets)
       (user-error "Unknown font preset %S" selection))
     (setq-local cat-font-buffer-preset selection))
    ((pred stringp)
     (let ((preset (intern selection)))
       (unless (assq preset cat-font-presets)
         (user-error "Unknown font preset %S" preset))
       (setq-local cat-font-buffer-preset preset))))
  (cat-font-refresh-buffer)
  (message "Buffer font preset: %s"
           (or cat-font-buffer-preset "base")))

(defun cat-font--refresh-after-local-variables ()
  "Refresh explicit buffer-local font configuration after loading it."
  (when (or (local-variable-p 'cat-font-buffer-preset)
            (local-variable-p 'cat-font-buffer-role-overrides))
    (cat-font-refresh-buffer)))

(add-hook 'cat-setup-fonts-hook #'cat--refresh-mode-fonts)
(add-hook 'window-buffer-change-functions #'cat--setup-window-fonts)
(add-hook 'after-change-major-mode-hook #'cat--setup-visible-mode-font)
(add-hook 'after-revert-hook #'cat--setup-visible-mode-font)
(add-hook 'hack-local-variables-hook
          #'cat-font--refresh-after-local-variables)

(when (and after-init-time (display-graphic-p))
  (cat-setup-fonts))
