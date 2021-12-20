(use-package org-cv
  :straight (org-cv :host gitlab :repo "Titan-C/org-cv")
  :defer t
  :after org
  :config
  (require 'ox-moderncv))
