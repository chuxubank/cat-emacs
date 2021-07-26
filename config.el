(cat! "+default")

;;; package
(cat! "+straight")
(cat! "+package")

;;; ui
(cat! "+font")
(when (featurep 'straight)
  (cat! "+nano"))
(cat! "+valign")

;;; edit
(cat! "+meow")
(cat! "+ctrlf")

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
(cat! "+pangu")

;;; app
(cat! "+telega")
(cat! "+pdf")
(cat! "+elfeed")

;;; util
(cat! "+project")
