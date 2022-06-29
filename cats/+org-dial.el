;; -*- lexical-binding: t; -*-

(use-package org-dial
  :straight (org-dial :host github :repo "mistrey/org-dial")
  :after org)

(when IS-MAC
  (setq org-dial-program "open tel:"))
