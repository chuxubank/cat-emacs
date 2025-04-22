;; -*- lexical-binding: t; -*-

(pretty-hydra-define cat-blog
  (:color teal :title (+with-icon "nf-fa-blog" "Blog"))
  ("Hugoista"
   (("h" #'hugoista "hugoista"))))

(use-package ox-hugo)

(use-package hugoista
  :custom
  (hugoista-site-dir cat-blog-directory))

(use-package easy-hugo
  :custom
  (easy-hugo-basedir cat-blog-directory)
  (easy-hugo-postdir "content/posts")
  (easy-hugo-default-ext ".org")
  :pretty-hydra
  (cat-blog
   ("EasyHugo"
    (("e" #'easy-hugo "easy-hugo")
     ("p" #'easy-hugo-preview "preview")))))
