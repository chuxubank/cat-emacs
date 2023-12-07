;; -*- lexical-binding: t; -*-

(use-package ox-confluence
  :disabled
  :ensure org-contrib
  :after org)

(use-package confluence
  :disabled
  :vc (:url "https://github.com/jahlborn/confluence-el" :rev :newest))

(use-package tributary
  :vc (:url "https://github.com/mrkrd/tributary" :rev :newest)
  :commands #'tributary-pull-id)

