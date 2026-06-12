;; -*- lexical-binding: t; -*-

(use-package corfu
  :hook (after-init . global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  :config
  (corfu-popupinfo-mode))

(use-package cape
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

(use-package nerd-icons-corfu
  :demand t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(defvar-keymap cat-cape-map
  :doc "Keymap for Cape."
  :name "Cat Cape"
  :prefix 'cat-cape-prefix
  "p"  #'completion-at-point
  "t"  #'complete-tag
  "d"  #'cape-dabbrev
  "h"  #'cape-history
  "f"  #'cape-file
  "k"  #'cape-keyword
  "s"  #'cape-elisp-symbol
  "e"  #'cape-elisp-block
  "a"  #'cape-abbrev
  "l"  #'cape-line
  "w"  #'cape-dict
  ":"  #'cape-emoji
  "\\" #'cape-tex
  "_"  #'cape-tex
  "^"  #'cape-tex
  "&"  #'cape-sgml
  "r"  #'cape-rfc1345)
