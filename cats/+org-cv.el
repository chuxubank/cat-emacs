;; -*- lexical-binding: t; -*-

(defvar cv-cwd cat-local-dir)

(use-package ox-altacv
  :vc (ox-altacv
       :url "https://gitlab.com/Titan-C/org-cv"
       :ignored-files "genfiles.el"
       :rev :newest)
  :demand t
  :after org)

(use-package ox-moderncv
  :vc (ox-moderncv
       :url "https://gitlab.com/Titan-C/org-cv"
       :ignored-files "genfiles.el"
       :rev :newest)
  :demand t
  :after org)

(use-package ox-awesomecv
  :vc (ox-awesomecv
       :url "https://gitlab.com/Titan-C/org-cv"
       :ignored-files "genfiles.el"
       :rev :newest)
  :demand t
  :after org)
