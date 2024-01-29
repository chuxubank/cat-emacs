(cat! "+package")
(cat! "+file")
(cat! "+utils")
(cat! "+benchmark")
(cat! "+default")
(cat! "+local")

;;; enhance
(cat! "+diredx")
(cat! "+tramp")
(cat! "+eudc")
(cat! "+sow")

;;; ui
(when (display-graphic-p)
  ;; (cat! "+nano")
  (cat! "+doom")
  (cat! "+font")
  (cat! "+valign")
  (cat! "+autodark"))
(cat! "+highlight")
(cat! "+windows")
(cat! "+treemacs")

;;; daemon
(cat! "+daemon")

;;; os
(when IS-MAC
  (cat! "+macos"))

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
(cat! "+tree-sitter")
(cat! "+doc")
(cat! "+sideline")
(cat! "+codeium")
;; (cat! "+lsp")
;; (cat! "+lsp-bridge")
(cat! "+eglot")

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
(cat! "+docker")
(cat! "+vb")
(cat! "+lua")

;;; plugins
(cat! "+term")
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
