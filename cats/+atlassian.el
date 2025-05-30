;; -*- lexical-binding: t; -*-

(use-package ox-confluence
  :disabled
  :ensure org-contrib
  :after org)

(use-package jira
  :pin melpa-stable
  :custom
  (jira-base-url (getenv "JIRA_URL")))

(use-package confluence
  :disabled
  :vc (confluence
       :url "https://github.com/jahlborn/confluence-el"
       :rev :newest))

(use-package tributary
  :vc (tributary
       :url "https://github.com/mrkrd/tributary"
       :rev :newest)
  :commands #'tributary-pull-id)

