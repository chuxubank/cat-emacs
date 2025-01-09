;; -*- lexical-binding: t; -*-

(use-package ox-altacv
  :vc (ox-altacv
       :url "https://gitlab.com/chuxubank/org-cv"
       :rev :newest)
  :demand t
  :after org)

(use-package ox-moderncv
  :vc (ox-moderncv
       :url "https://gitlab.com/chuxubank/org-cv"
       :rev :newest)
  :demand t
  :after org)

(use-package ox-awesomecv
  :vc (ox-awesomecv
       :url "https://gitlab.com/chuxubank/org-cv"
       :rev :newest)
  :demand t
  :after org)
