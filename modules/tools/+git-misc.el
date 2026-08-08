;; -*- lexical-binding: t; -*-

(mode-transient-define-prefix cat-git-misc ()
  :description (+with-icon "nf-md-git" nil " Git misc"))

(use-package git-timemachine
  :delight (git-timemachine-mode
            (:eval (+with-icon "nf-cod-history" " ")))
  :transient
  (cat-git-misc
   ["Git History"
    ("t" "git timemachine" git-timemachine-toggle)]))

(use-package git-link
  :custom
  (git-link-open-in-browser t)
  :transient
  (cat-git-misc
   ["Git Link"
    ("l" "git link" git-link-dispatch)]))

(use-package code-review
  :vc (code-review :url "https://github.com/doomelpa/code-review")
  :transient
  (cat-git-misc
   ["Code Review"
    ("r" "code review forge" code-review-forge-pr-at-point)
    ("R" "code review start" code-review-start)]))
