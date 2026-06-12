;; -*- lexical-binding: t; -*-

(pretty-hydra-define cat-oj
  (:color teal :title (+with-icon "nf-md-code_tags_check" "Online Judging"))
  ("" ()))

(use-package leetcode
  :custom
  (leetcode-python-environment (concat cat-etc-dir "leetcode-env"))
  (leetcode-prefer-language "kotlin")
  :pretty-hydra
  (cat-oj
   ("LeetCode"
    (("l" #'leetcode "leetcode")
     ("d" #'leetcode-daily "daily")
     ("k" #'leetcode-quit "quit")))))

(use-package leetcode-org-roam
  :ensure nil
  :bind
  (:map leetcode--problems-mode-map
        ("C-c C-c" . leetcode-org-roam-capture)))

(use-package oj
  :disabled
  :pretty-hydra
  (cat-oj
   ("Online Judge Tools"
    (("o" #'oj-prepare "prepare")))))
