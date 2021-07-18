(cat! "+default")

;;; package
(cat! "+straight")
(cat! "+package")

;;; theme
(cat! "+font")
(when (featurep 'straight)
  (cat! "+nano"))

;;; edit
(cat! "+meow")

;;; completion
(cat! "+company")
(cat! "+rg")
(cat! "+orderless")
(cat! "+selectrum")

;;; git
(cat! "+magit")

;;; org
(cat! "+org")
(cat! "+org-roam")

;;; input
(cat! "+rime")

;;; app
(cat! "+telega")
(cat! "+pdf")
(cat! "+elfeed")

;;; defaults
(cat! "+project")
(cat! "+map")
