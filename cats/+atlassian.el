;; -*- lexical-binding: t; -*-

(use-package ox-confluence
  :disabled
  :ensure org-contrib
  :after org)

(use-package jira
  :pin melpa-stable
  :custom
  (jira-base-url (getenv "JIRA_URL"))
  (jira-detail-show-announcements nil))

(use-package task
  :commands #'task-start-dev-work
  :ensure nil
  :custom
  (task-jira-default-jql "assignee = currentUser() AND Sprint in (openSprints(), futureSprints()) AND resolution = Unresolved order by updated DESC"))

(use-package confluence
  :disabled
  :vc (:url "https://github.com/jahlborn/confluence-el"))

(use-package tributary
  :vc (:url "https://github.com/mrkrd/tributary")
  :commands #'tributary-pull-id)

