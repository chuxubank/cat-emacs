;; -*- lexical-binding: t; -*-

(use-package corfu
  :pin gnu
  :hook (after-init . global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  :config
  (corfu-popupinfo-mode))

(use-package cape
  :bind
  ("M-p t" . complete-tag)
  ("M-p d" . cape-dabbrev)
  ("M-p h" . cape-history)
  ("M-p f" . cape-file)
  ("M-p k" . cape-keyword)
  ("M-p s" . cape-elisp-symbol)
  ("M-p e" . cape-elisp-block)
  ("M-p a" . cape-abbrev)
  ("M-p l" . cape-line)
  ("M-p w" . cape-dict)
  ("M-p :" . cape-emoji)
  ("M-p \\" . cape-tex)
  ("M-p _" . cape-tex)
  ("M-p ^" . cape-tex)
  ("M-p &" . cape-sgml)
  ("M-p r" . cape-rfc1345)
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
