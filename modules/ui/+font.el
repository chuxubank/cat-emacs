;; -*- lexical-binding: t; -*-

(use-package prosody
  :vc (:url "https://github.com/cat-emacs/prosody")
  :demand t
  :init
  (setq use-default-font-for-symbols (not STIPPLE-COMPATIBLE-P))
  :config
  (require 'prosody-use-package)
  (require 'prosody-nerd-icons)
  (add-hook 'cat-theme-refresh-hook #'prosody-setup))

(if IS-MACPORT
    (mac-auto-operator-composition-mode)
  (use-package ligature
    :hook (after-init . global-ligature-mode)
    :config
    (ligature-set-ligatures 't '("www"
                                 "[TODO]" "todo))"
                                 "[FIXME]" "fixme))"
                                 "[DEBUG]" "[INFO]" "[WARN]" "[ERROR]"))
    (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
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

(setq face-font-rescale-alist
      '(("Noto Serif Thai" . 0.4)
        ("Noto Naskh Arabic" . 0.4)
        ("Math" . 0.7)
        ("Noto Sans .+" . 0.7)
        ("Apple Color Emoji" . 0.8)
        ("Sinhala Sangam MN" . 0.8)
        ("Apple Symbols" . 0.9)
        ("Apple Chancery" . 0.9)
        ("Noto Serif .+" . 0.9)
        ("Source Han .+" . 0.9)
        ("Zhuque Fangsong .+" . 0.9)
        ("-cdac$" . 1.3)))
