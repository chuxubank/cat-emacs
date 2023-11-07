(use-package org-cv
  :vc (:url "https://github.com/Titan-C/org-cv" :rev :newest)
  :defer t
  :after org
  :config
  (require 'ox-moderncv)
  (require 'ox-awesomecv))
