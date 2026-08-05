;; -*- lexical-binding: t; -*-

(defgroup cat-font nil
  "Font settings for Cat Emacs."
  :group 'cat)

(defcustom cat-font-stacks
  '((fallback
     :symbol ("Apple Symbols" "Symbola")
     :mathematical ("STIX Two Math"
                    "DejaVu Math TeX Gyre"
                    "Noto Sans Math")
     :emoji ("Apple Color Emoji"))
    (sans-serif-ui
     :extends fallback
     :ascii ("Inter" "Avenir Next" "DejaVu Sans")
     :cjk ("PingFang SC" "Hiragino Sans GB" "Noto Sans CJK SC"
           "Source Han Sans SC" "Microsoft YaHei"))
    (serif
     :extends fallback
     :ascii ("Charter" "Roboto Serif" "DejaVu Serif" "Georgia")
     :cjk ("Songti SC" "LXGW WenKai" "Noto Serif CJK SC"
           "Source Han Serif SC"))
    (slab-serif
     :extends serif
     :ascii ("Roboto Slab" "American Typewriter"))
    (quasi-proportional
     :extends serif
     :ascii ("Iosevka Etoile" "Iosevka Aile"))
    (monospace-narrow
     :extends fallback
     :ascii ("Iosevka" "Iosevka Term")
     :symbol ("Iosevka")
     :cjk ("LXGW WenKai Mono" "Sarasa Mono SC"))
    (monospace-code
     :extends monospace-narrow
     :ascii ("Maple Mono" "Source Code Pro"))
    (monospace-sans-serif
     :extends monospace-narrow
     :ascii ("Roboto Mono" "DejaVu Sans Mono")))
  "Physical font candidates grouped into reusable stacks.
Each entry has the form (STACK :CATEGORY FONTS...).  Categories include
:ascii, :cjk, :symbol, :mathematical, and :emoji.  A stack can inherit
missing properties from another stack with :extends."
  :type 'sexp)

(defcustom cat-font-preset
  `((default :stack monospace-narrow
             :height ,(if IS-MAC 160 140))
    (heading :stack serif :weight semi-bold)
    (title :extends heading :height 1.4)
    (body :stack monospace-sans-serif)
    (documentation :extends body)
    (prose :stack quasi-proportional)
    (ui :stack sans-serif-ui)
    (metadata-label :extends ui)
    (metadata-value :stack monospace-narrow)
    (mono :stack monospace-sans-serif)
    (code :stack monospace-code)
    (table :extends mono :stack monospace-narrow)
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
  :type 'sexp)

(defcustom cat-font-script-rules
  '(((han kana hangul bopomofo cjk-misc) cjk)
    (symbol symbol)
    (mathematical mathematical)
    (emoji emoji))
  "Map fontset character targets to stack categories.
Each rule has the form (CHARACTERS CATEGORY &optional ADD).  CHARACTERS
can be a script symbol or a list of script symbols.  CATEGORY names a
font category configured in `cat-font-stacks'."
  :type 'sexp)

(setq use-default-font-for-symbols (not STIPPLE-COMPATIBLE-P))

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
:font        Font role, concrete family, or ordered family list.
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

(defun cat--font-stack-spec (stack)
  "Return the inherited specification for font STACK."
  (when (symbolp stack)
    (let* ((spec (alist-get stack cat-font-stacks))
           (parent (plist-get spec :extends)))
      (unless spec
        (error "Unknown font stack: %S" stack))
      (if parent
          (cat--merge-font-attributes (cat--font-stack-spec parent) spec)
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

(defun cat--font-role-face (role)
  "Return the face owned by font ROLE, creating it when necessary."
  (when (cat--font-role-spec role)
    (let ((face (intern (format "cat-font-role-%s" role))))
      (unless (facep face)
        (make-empty-face face))
      face)))

(defun cat--configure-font-role-face (role &optional frame fontset)
  "Configure ROLE's face on FRAME, optionally using FONTSET."
  (let ((face (cat--font-role-face role)))
    ;; `:font' resolves the Latin font; `:fontset' restores script mappings.
    (apply #'set-face-attribute face frame
           (append (when fontset (list :font fontset :fontset fontset))
                   (cat--font-role-attributes role)))
    face))

(dolist (role cat-font-preset)
  (cat--configure-font-role-face (car role)))

(defvar cat--fontset-signatures (make-hash-table :test 'eq)
  "Last configured signature for each Cat role fontset.")

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

(defun cat--fontset-name (role)
  "Return the fontset name owned by ROLE."
  (format "-*-cat-*-*-*-*-*-*-*-*-*-*-fontset-cat_%s"
          (replace-regexp-in-string "-" "_" (symbol-name role))))

(defun cat--fontset-signature (role)
  "Return the configuration signature for ROLE's fontset."
  (list
   (cat--font-role-candidates role 'ascii)
   (cat--font-rules role)))

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
    (cat-benchmark 'beg "setup fonts.")
    ;; Configure all role fontsets before their scripts are first displayed.
    (dolist (role cat-font-preset)
      (let* ((name (car role))
             (fontset (cat--fontset-for-role name frame)))
        (cat--configure-font-role-face name frame fontset)))
    (when-let* ((fontset (cat--fontset-for-role 'default frame)))
      (apply #'set-face-attribute 'default frame
             :font fontset :fontset fontset
             (cat--font-role-attributes 'default)))
    (cat--configure-default-fontset
     (cat--fontset-signature 'default) frame)
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

(add-hook 'cat-setup-fonts-hook #'cat--refresh-mode-fonts)
(add-hook 'window-buffer-change-functions #'cat--setup-window-fonts)
(add-hook 'after-change-major-mode-hook #'cat--setup-visible-mode-font)
(add-hook 'after-revert-hook #'cat--setup-visible-mode-font)

(when (and after-init-time (display-graphic-p))
  (cat-setup-fonts))
