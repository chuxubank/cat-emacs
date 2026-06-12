;; -*- lexical-binding: t; -*-

(use-package org-transclusion
  :demand t
  :after org
  :custom
  (org-transclusion-extensions '(org-transclusion-src-lines
                                 org-transclusion-indent-mode)))
