;;; custom.el --- Default Cat Emacs custom settings -*- lexical-binding: t; -*-

;;; Commentary:

;; This file is the fallback Custom file for Cat Emacs.  User customizations
;; should live in ~/.config/cat-emacs/custom.el.

;;; Code:

(custom-set-variables
 '(gptel-model-updater-backends
   '(gptel--gemini gptel--llama gptel--mlx gptel--ollama
                   gptel--openrouter))
 '(gptel-model-updater-external-targets
   '((gptel-magit-backend gptel-magit-model "GPTel-Magit"
                          ("OpenRouter:openai/gpt-oss-120b:free"))
     (gptel-forge-prs-backend gptel-forge-prs-model "GPTel-Forge-Prs"
                              ("OpenRouter:openai/gpt-oss-120b:free"))))
 '(gptel-model-updater-models '("OpenRouter:auto"))
 '(use-short-answers t)
 '(package-native-compile t)
 '(system-packages-use-sudo nil)
 '(package-vc-selected-packages
   '((android-mode :url "https://github.com/chuxubank/emacs-studio"
                   :lisp-dir "android-mode/")
     (code-review :url "https://github.com/doomelpa/code-review")
     (compose-preview :url "https://github.com/chuxubank/emacs-studio"
                      :lisp-dir "compose-preview/")
     (diagram-preview :url "https://github.com/natrys/diagram-preview")
     (elogcat :url "https://github.com/chuxubank/elogcat.el")
     (gptel-model-updater :url
                          "https://github.com/chuxubank/gptel-model-updater")
     (gptel-prompts :url "https://github.com/jwiegley/gptel-prompts")
     (gradle-el :url "https://git.sr.ht/~vhallac/gradle-el")
     (go-template-ts-mode :url
                          "https://github.com/chuxubank/go-template-ts-mode")
     (hydra-posframe :url "https://github.com/Ladicle/hydra-posframe")
     (jinja2-ts-mode :url
                     "https://github.com/chuxubank/jinja2-ts-mode")
     (kitty-graphics :url
                     "https://github.com/cashmeredev/kitty-graphics.el")
     (lsp-proxy :url "https://github.com/jadestrong/lsp-proxy")
     (magit-difftastic :url
                       "https://github.com/rschmukler/magit-difftastic")
     (markdown-xwidget :url
                       "https://github.com/cfclrk/markdown-xwidget")
     (math-delimiters :url
                      "https://github.com/oantolin/math-delimiters")
     (mcp-server :url "https://github.com/rhblind/emacs-mcp-server")
     (md-babel :url "https://github.com/md-babel/md-babel.el")
     (netease-cloud-music :url
                          "https://github.com/chuxubank/netease-cloud-music.el")
     (ob-async :url "https://github.com/ezchi/ob-async" :branch
               "develop")
     (ob-kotlin :url "https://github.com/chuxubank/ob-kotlin")
     (org-agenda-count :url
                       "https://github.com/sid-kurias/org-agenda-count")
     (org-cv :url "https://gitlab.com/Titan-C/org-cv")
     (org-dial :url "https://github.com/mistrey/org-dial")
     (org-fc :url "https://github.com/l3kn/org-fc")
     (org-imgtog :url "https://github.com/gaoDean/org-imgtog")
     (org-media-note :url
                     "https://github.com/yuchen-lea/org-media-note")
     (org-mode :url "https://code.tecosaur.net/tec/org-mode" :lisp-dir
               "lisp/" :branch "dev")
     (org-remoteimg :url "https://github.com/gaoDean/org-remoteimg")
     (org-yt :url "https://github.com/TobiasZawada/org-yt")
     (poly-any-go-template :url
                          "https://github.com/chuxubank/poly-any-template"
                          :lisp-dir "lisp/go-template")
     (chezmoi :url "https://github.com/chuxubank/chezmoi.el")
     (poly-any-jinja2 :url
                      "https://github.com/chuxubank/poly-any-template"
                      :lisp-dir "lisp/jinja2")
     (poly-any-template :url
                        "https://github.com/chuxubank/poly-any-template"
                        :lisp-dir "lisp/shared")
     (poly-treesit-fold :url
                        "https://github.com/chuxubank/poly-any-template"
                        :lisp-dir "lisp/treesit-fold")
     (treemacs-activities :url
                          "https://github.com/chuxubank/treemacs-activities")
     (ultra-scroll :url "https://github.com/jdtsmith/ultra-scroll")
     (vbnet-mode :url "https://github.com/lelit/vbnet-mode")
     (video-trimmer :url "https://github.com/xenodium/video-trimmer")))
 '(package-pinned-packages
   '((transient . "gnu")
     (eglot . "gnu")
     (magit . "melpa-stable")
     (magit-section . "melpa-stable")
     (forge . "melpa-stable")
     (sideline . "melpa-stable")
     (jira . "melpa-stable")
     (markdown-mode . "melpa-stable")
     (copilot . "melpa-stable")
     (org-link-beautify . "melpa-stable")
     (ts-docstr . "jcs-elpa")
     (ts-fold . "jcs-elpa")
     (sideline-eglot . "jcs-elpa")
     (rainbow-csv . "jcs-elpa")
     (treesit-fold . "jcs-elpa")
     (vbs-repl . "jcs-elpa")
     (visual-basic-mode . "jcs-elpa")
     (vbscript-mode . "jcs-elpa")
     (beancount . "jcs-elpa")
     (indent-bars . "jcs-elpa")
     (org-modern-indent . "jcs-elpa")
     (codeium . "jcs-elpa")))
 '(package-selected-packages
   '(ace-pinyin
     ace-window
     activities
     agent-shell
     aider
     aidermacs
     anki-vocabulary
     ansible
     ansible-doc
     apheleia
     applescript-mode
     auctex
     avy
     avy-embark-collect
     bbdb
     beacon
     beancount
     benchmark-init
     bibtex-completion
     bing-dict
     breadcrumb
     bufler
     burly
     caddyfile-mode
     cape
     cdlatex
     chatgpt-shell
     citar
     citar-embark
     citar-org-roam
     cmake-font-lock
     cmake-mode
     codeium
     compile-multi
     compile-multi-embark
     compile-multi-nerd-icons
     consult
     consult-compile-multi
     consult-gh
     consult-gh-embark
     consult-gh-forge
     corfu
     corfu-prescient
     csv-mode
     dart-mode
     dashboard
     deft
     delight
     devdocs
     diff-hl
     dired-rsync
     dired-rsync-transient
     dirvish
     docker
     docker-compose-mode
     dockerfile-mode
     doom-modeline
     doom-themes
     dumb-jump
     easy-hugo
     eglot
     eldoc-box
     eldoc-toml
     elfeed
     elfeed-org
     elfeed-protocol
     elisp-demos
     emacs-everywhere
     embark
     embark-consult
     ement
     emms
     eshell-vterm
     exec-path-from-shell
     flycheck
     flycheck-gradle
     flycheck-kotlin
     flycheck-plantuml
     flycheck-pos-tip
     flycheck-posframe
     flymake-ansible-lint
     flymake-gradle
     forge
     ghostel
     git-link
     git-timemachine
     go-mode
     go-template-ts-mode
     jinja2-ts-mode
     goggles
     golden-ratio
     gptel
     gptel-forge-prs
     gptel-magit
     graphql-mode
     grip-mode
     groovy-mode
     gt
     helpful
     hugoista
     igist
     immersive-translate
     indent-bars
     jabber
     kaolin-themes
     kdl-mode
     kotlin-mode
     kotlin-ts-mode
     leetcode
     ligature
     logview
     lua-mode
     magit
     magit-section
     magit-todos
     major-mode-hydra
     makefile-executor
     marginalia
     markdown-mode
     meow
     meow-tree-sitter
     mermaid-mode
     minimap
     mistty
     mpv
     mu4e-column-faces
     mu4e-overview
     mustache
     nerd-icons
     nerd-icons-completion
     nerd-icons-corfu
     nerd-icons-dired
     no-littering
     nov
     ob-aider
     ob-applescript
     ob-go
     ob-powershell
     ob-rust
     ob-swiftui
     ob-typescript
     org-anki
     org-appear
     org-cliplink
     org-drill
     org-edna
     org-link-beautify
     org-modern
     org-modern-indent
     org-noter
     org-pdftools
     org-roam
     org-roam-bibtex
     org-roam-ui
     osm
     osx-dictionary
     ox-hugo
     pangu-spacing
     pass
     password-store
     password-store-menu
     pdf-tools
     pet
     pinyin-isearch
     pinyin-search
     plantuml-mode
     poetry
     poly-any-go-template
     poly-any-jinja2
     poly-any-template
     poly-treesit-fold
     polymode
     powershell
     promise
     protobuf-mode
     pulsar
     rainbow-csv
     rainbow-mode
     rg
     rime
     rust-mode
     shell-maker
     sideline
     sideline-blame
     sideline-eglot
     sideline-flycheck
     sideline-flymake
     smartparens
     solaire-mode
     swift-mode
     swift-ts-mode
     system-packages
     tabspaces
     telega
     templ-ts-mode
     templatel
     toc-mode
     transient
     transpose-frame
     treemacs
     treemacs-magit
     treemacs-nerd-icons
     treemacs-tab-bar
     treesit-auto
     treesit-fold
     treesit-langs
     undo-fu-session
     uv-mode
     valign
     vbs-repl
     vbscript-mode
     vertico
     vertico-prescient
     visual-basic-mode
     vlf
     vterm
     vterm-toggle
     vundo
     which-key
     winum
     yaml-pro
     yasnippet)))

;;; custom.el ends here
