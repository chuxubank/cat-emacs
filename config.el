(cat! "+package")
(cat! "+utils")
(cat! "+benchmark")
(cat! "+default")
(cat! "+env")
(cat! "+local")

;;; enhance
(cat! "+diredx")
(cat! "+tramp")
(cat! "+eudc")

;;; ui
;; (cat! "+nano")
(cat! "+doom")
(cat! "+font")
(cat! "+autodark")
(cat! "+beauty")
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
(cat! "+org-roam")
(cat! "+org-notes")
(cat! "+org-latex")
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
(cat! "+android")
(cat! "+applescript")
(cat! "+cmake")
(cat! "+dart")
(cat! "+go")
(cat! "+gradle")
(cat! "+js")
(cat! "+jvm")
(cat! "+lua")
(cat! "+makefile")
(cat! "+mermaid")
(cat! "+plantuml")
(cat! "+powershell")
(cat! "+protobuf")
(cat! "+python")
(cat! "+swift")
(cat! "+vb")
(cat! "+yaml")

;;; tools
(cat! "+ansible")
(cat! "+caddy")
(cat! "+docker")

;;; plugins
(cat! "+term")
(cat! "+log")
(cat! "+im")
(cat! "+pdf")
(cat! "+nov")
(cat! "+toc")
(cat! "+elfeed")
(cat! "+language")
(cat! "+ncm")
(cat! "+mail")
(cat! "+music")
(cat! "+pass")
(cat! "+gist")
(cat! "+beancount")
(cat! "+diagram-preview")
(cat! "+confluence")
(cat! "+chezmoi")
(cat! "+undo")

(cat! "+keymap")
