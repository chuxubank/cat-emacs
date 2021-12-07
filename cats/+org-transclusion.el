;; -*- lexical-binding: t; -*-

(use-package org-transclusion
  :straight (org-transclusion
	     :host github
	     :repo "nobiot/org-transclusion"
	     :files ("*.el"))
  :after org
  :custom
  (org-transclusion-extensions '(org-transclusion-src-lines
				 org-transclusion-indent-mode)))
