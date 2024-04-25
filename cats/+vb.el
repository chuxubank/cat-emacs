;; -*- lexical-binding: t; -*-

(use-package vbscript-mode
  :pin jcs-elpa
  :mode "\\.\\(bas\\|vb\\)$")

(use-package vbnet-mode
  :disabled
  :vc (vbnet-mode
       :url "https://github.com/lelit/vbnet-mode"
       :rev :newest)
  :mode "\\.\\(frm\\|bas\\|cls\\|vb\\)$"
  :custom
  (vbnet-want-flymake-fixup nil))
