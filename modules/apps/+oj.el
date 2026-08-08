;; -*- lexical-binding: t; -*-

(mode-transient-define-prefix cat-oj ()
  :description (+with-icon "nf-md-code_tags_check" nil " Online Judging"))

(use-package leetcode
  :custom
  (leetcode-python-environment (concat cat-etc-dir "leetcode-env"))
  (leetcode-prefer-language "kotlin")
  :transient
  (cat-oj
   ["LeetCode"
    ("l" "leetcode" leetcode)
    ("d" "daily" leetcode-daily)
    ("k" "quit" leetcode-quit)]))

(use-package leetcode-org-roam
  :vc (:url "https://github.com/cat-emacs/leetcode-org-roam")
  :bind
  (:map leetcode--problems-mode-map
        ("C-c C-c" . leetcode-org-roam-capture)))

(use-package oj
  :cat cli
  :transient
  (cat-oj
   ["Online Judge Tools"
    ("o" "prepare" oj-prepare)]))
