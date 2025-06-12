;; -*- lexical-binding: t; -*-

(pretty-hydra-define cat-oj
  (:color teal :title (+with-icon "nf-md-code_tags_check" "Online Judging"))
  ("" ()))

(use-package leetcode
  :pretty-hydra
  (cat-oj
   ("leetcode"
    (("l" #'leetcode "leetcode")
     ("d" #'leetcode-daily "leetcode daily")))))

(use-package oj
  :disabled
  :pretty-hydra
  (cat-oj
   ("Online Judge Tools"
    (("o" #'oj-prepare "oj prepare")))))
