;; -*- lexical-binding: t; -*-

(use-package vbscript-mode
  :pin jcs-elpa
  :mode "\\.\\(bas\\|vb\\)$")

(use-package vbnet-mode
  :disabled
  :vc (:url "https://github.com/lelit/vbnet-mode")
  :mode "\\.\\(frm\\|bas\\|cls\\|vb\\)$"
  :custom
  (vbnet-want-flymake-fixup nil))
