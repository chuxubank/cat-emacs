;; -*- lexical-binding: t; -*-

(use-package ox-jira
  :after org)

(use-package org-jira
  :after org
  :config
  (make-directory org-jira-working-dir))
