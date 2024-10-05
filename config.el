(cat! "+package")
(cat! "+utils")
(cat! "+file")
(cat! "+benchmark")
(cat! "+default")
(cat! "+env")
(cat! "+local")

;;; enhance
(cat! "+diredx")
(cat! "+tramp")
(cat! "+eudc")
(cat! "+sow")

;;; ui
;; (cat! "+nano")
(cat! "+doom")
(cat! "+valign")
(cat! "+font")
(cat! "+autodark")
(cat! "+highlight")
(cat! "+windows")
(cat! "+workspace")
(cat! "+treemacs")
(cat! "+dashboard")

;;; daemon
(cat! "+daemon")

;;; os
(when IS-MAC
  (cat! "+macos"))

;;; editor
(cat! "+meow")
(cat! "+avy")
(cat! "+smartparens")
;; (cat! "+hydra")

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
(cat! "+git-misc")

;;; org
(cat! "+org")
(cat! "+org-todo")
(cat! "+org-notes")
(cat! "+org-latex")
(cat! "+org-roam")
(cat! "+org-srs")
;; (cat! "+org-cv")
(cat! "+org-link")
(cat! "+org-jira")

;;; latex
(cat! "+bibcite")
(cat! "+latex")
(cat! "+cdlatex")
;; (cat! "+zotero")

;;; markdown
(cat! "+markdown")

;;; csv
(cat! "+csv")

;;; input
(cat! "+rime")

;;; code
(cat! "+format")
(cat! "+flycheck")
(if EMACS29+ (cat! "+treesit") (cat! "+tree-sitter"))
(cat! "+doc")
(cat! "+sideline")
(cat! "+codeium")
;; (cat! "+lsp")
;; (cat! "+lsp-bridge")
(cat! "+eglot")
(cat! "+jump")

;;; language
(cat! "+cmake")
(cat! "+makefile")
(cat! "+powershell")
(cat! "+yaml")
(cat! "+kotlin")
(cat! "+android")
(cat! "+swift")
(cat! "+gradle")
(cat! "+python")
(cat! "+dart")
(cat! "+plantuml")
(cat! "+mermaid")
(cat! "+protobuf")
(cat! "+js")
(cat! "+vb")
(cat! "+lua")
(cat! "+go")

;;; tools
(cat! "+ansible")
(cat! "+caddy")
(cat! "+docker")

;;; plugins
(cat! "+term")
(cat! "+log")
(cat! "+chat")
(cat! "+pdf")
(cat! "+nov")
(cat! "+toc")
(cat! "+elfeed")
(cat! "+language")
(cat! "+ncm")
(cat! "+mu4e")
(cat! "+music")
(cat! "+pass")
(cat! "+gist")
(cat! "+beancount")
(cat! "+diagram-preview")
(cat! "+confluence")
(cat! "+chezmoi")
(cat! "+undo")

(cat! "+keymap")
