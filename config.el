(cat! "+default")

;;; package
(cat! "+package")
(cat! "+benchmark")
(cat! "+straight")

;;; util
(cat! "+project")
(cat! "+diredx")
(cat! "+eudc")
(cat! "+sow")
(cat! "+utils")

;;; ui
(cat! "+font")
(cat! "+valign")
(cat! "+windows")
(cat! "+local")
(cat! "+treemacs")

;;; os
(when IS-MAC
  (cat! "+macos"))

;;; daemon
(when (daemonp)
  (cat! "+daemon"))

;;; theme
;; (cat! "+nano")
(cat! "+doom")
(cat! "+autodark")
(cat! "+highlight")

;;; edit
(cat! "+meow")
(cat! "+avy")
(cat! "+smartparens")

;;; completion
;; (cat! "+company")
(cat! "+corfu")
;; (cat! "+selectrum")
(cat! "+vertico")
;; (cat! "+orderless")
(cat! "+prescient")
(cat! "+marginalia")
(cat! "+which-key")
(cat! "+consult")
(cat! "+embark")
(cat! "+yasnippet")

;;; search
(cat! "+rg")

;;; git
(cat! "+magit")

;;; org
(cat! "+org")
(cat! "+org-todo")
(cat! "+org-notes")
(cat! "+org-latex")
(cat! "+org-roam")
(cat! "+org-srs")
(cat! "+org-cv")
(cat! "+org-link")
(cat! "+org-jira")

;;; latex
(cat! "+bibcite")
(cat! "+latex")
(cat! "+cdlatex")
;; (cat! "+zotero")

;;; input
(cat! "+rime")

;;; code
(cat! "+format")
(cat! "+flycheck")
(cat! "+doc")
(cat! "+sideline")
;; (cat! "+lsp")
;; (cat! "+lsp-bridge")
(cat! "+codeium")

;;; language
(cat! "+cmake")
(cat! "+powershell")
(cat! "+yaml")
(cat! "+kotlin")
(cat! "+android")
(cat! "+gradle")
(cat! "+python")
(cat! "+dart")
(cat! "+plantuml")
(cat! "+mermaid")
(cat! "+json")
(cat! "+caddy")

;;; plugins
(cat! "+log")
(cat! "+telega")
(cat! "+pdf")
(cat! "+nov")
(cat! "+toc")
(cat! "+elfeed")
(cat! "+dict")
(cat! "+ncm")
(cat! "+mu4e")
(cat! "+pass")
(cat! "+gist")
(cat! "+beancount")
(cat! "+diagram-preview")
(cat! "+confluence")
(cat! "+chezmoi")
(cat! "+undo")

(cat! "+keymap")
