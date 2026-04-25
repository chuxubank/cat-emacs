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
(cat! "+term")

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
(when (daemonp) (cat! "+daemon"))

;;; os
(when IS-MAC (cat! "+macos"))

;;; editor
(cat! "+meow")
(cat! "+avy")
(cat! "+smartparens")
;; (cat! "+hydra")

;;; completion
;; (cat! "+company")
(cat! "+corfu")
(cat! "+vertico")
;; (cat! "+orderless")
(cat! "+prescient")
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
(cat! "+org-babel")
(cat! "+org-todo")
(cat! "+org-roam")
(cat! "+org-notes")
(cat! "+org-latex")
(cat! "+org-srs")
(cat! "+org-cv")
(cat! "+org-link")

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
;; (cat! "+lsp")
;; (cat! "+lsp-bridge")
(cat! "+eglot")

;;; ai
(cat! "+copilot")
(cat! "+vibe")

;;; program
(cat! "+android")
(cat! "+applescript")
(cat! "+cmake")
(cat! "+dart")
(cat! "+go")
(cat! "+gradle")
(cat! "+graphql")
;; (cat! "+java")
(cat! "+kotlin")
(cat! "+kdl")
(cat! "+lua")
(cat! "+makefile")
(cat! "+mermaid")
(cat! "+node")
(cat! "+plantuml")
(cat! "+poly")
(cat! "+protobuf")
(cat! "+python")
(cat! "+rust")
(cat! "+shell")
(cat! "+swift")
(cat! "+vb")
(cat! "+web")
(cat! "+yaml")

;;; tools
(cat! "+ansible")
(cat! "+caddy")
(cat! "+docker")

;;; plugins
(when (eq HOST_ENV 'aa)
  (cat! "+atlassian")
  (cat! "+org-jira"))
(cat! "+beancount")
(cat! "+blog")
(cat! "+chezmoi")
(cat! "+diagram-preview")
(cat! "+elfeed")
(cat! "+github")
(cat! "+im")
(cat! "+language")
(cat! "+log")
(cat! "+mail")
(cat! "+map")
(cat! "+media")
(cat! "+music")
(cat! "+nov")
(cat! "+oj")
(cat! "+pass")
(cat! "+pdf")
(cat! "+toc")
(cat! "+undo")

(cat! "+keymap")
