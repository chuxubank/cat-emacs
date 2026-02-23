;; -*- lexical-binding: t; -*-

(pretty-hydra-define cat-git-misc
  (:color teal :title (+with-icon "nf-md-git" "Git misc"))
  ("" ()))

(use-package git-timemachine
  :delight " "
  :pretty-hydra
  (cat-git-misc
   ("Git History"
    (("t" #'git-timemachine-toggle "git timemachine")))))

(use-package git-link
  :custom
  (git-link-open-in-browser t)
  :pretty-hydra
  (cat-git-misc
   ("Git Link"
    (("l" #'git-link-dispatch "git link")))))

(use-package code-review
  :vc (code-review :url "https://github.com/doomelpa/code-review")
  :pretty-hydra
  (cat-git-misc
   ("Code Review"
    (("r" #'code-review-forge-pr-at-point "code review forge")
     ("R" #'code-review-start "code review start")))))
