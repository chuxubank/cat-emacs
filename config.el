(cat! "+default")

;;; package
(cat! "+package")
(cat! "+benchmark")
(cat! "+straight")

;;; util
(cat! "+project")
(cat! "+diredx")
(cat! "+sow")
(cat! "+utils")

;;; ui
(cat! "+font")
(cat! "+valign")
(cat! "+transpose-frame")

;;; os
(when IS-MAC
  (cat! "+macos"))

;;; daemon
(when (daemonp)
  (cat! "+daemon"))

;;; theme
(cat! "+nano")
(cat! "+autodark")
(cat! "+highlight")

;;; edit
(cat! "+meow")
(cat! "+smartparens")

;;; completion
(cat! "+company")
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
(cat! "+ctrlf")
(cat! "+rg")

;;; git
(cat! "+magit")

;;; org
(cat! "+org")
(cat! "+org-notes")
(cat! "+citar")
(cat! "+org-latex")
(cat! "+org-roam")
(cat! "+orb-notes")
(cat! "+org-srs")
(cat! "+org-cv")
(cat! "+org-yt")
(cat! "+org-dial")
(cat! "+org-jira")

;;; latex
(cat! "+latex")
(cat! "+cdlatex")
;; (cat! "+zotero")

;;; input
(cat! "+rime")
(cat! "+pangu")

;;; program
;; (cat! "+format-all")
(cat! "+apheleia")
(cat! "+lsp")
(cat! "+flycheck")
;; (cat! "+copilot")
(cat! "+yaml")
(cat! "+kotlin")
(cat! "+gradle")
(cat! "+python")
(cat! "+dart")
(cat! "+plantuml")
(cat! "+mermaid")
(cat! "+json")
(cat! "+caddy")

;;; plugins
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

(cat! "+keymap")
