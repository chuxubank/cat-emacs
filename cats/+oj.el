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
     ("d" #'leetcode-daily "leetcode daily")
     ("k" #'leetcode-quit "leetcode quit")))))

(use-package oj
  :disabled
  :pretty-hydra
  (cat-oj
   ("Online Judge Tools"
    (("o" #'oj-prepare "oj prepare")))))
