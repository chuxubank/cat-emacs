;; -*- lexical-binding: t; -*-

(use-package vbscript-mode
  :vc (vbscript-mode
       :url "https://github.com/nverno/vbs-mode"
       :rev :newest)
  :mode "\\.\\(bas\\|vb\\)$")

(use-package vbnet-mode
  :disabled
  :vc (vbnet-mode
       :url "https://github.com/lelit/vbnet-mode"
       :rev :newest)
  :mode "\\.\\(frm\\|bas\\|cls\\|vb\\)$"
  :custom
  (vbnet-want-flymake-fixup nil))
