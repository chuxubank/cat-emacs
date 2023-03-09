;; -*- lexical-binding: t; -*-

(use-package ox-confluence
  :disabled
  :ensure org-contrib
  :after org)

(use-package confluence
  :disabled
  :straight (confluence
	     :host github
	     :repo "jahlborn/confluence-el"
	     :files (:defaults "confluence.dtd" "confluence2wiki.xsl"))
  :defer t)

(use-package tributary
  :straight (tributary
	     :host github
	     :repo "mrkrd/tributary"
	     :files (:defaults "confluence.rnc"))
  :defer t)

