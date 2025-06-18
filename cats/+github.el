;; -*- lexical-binding: t; -*-

(pretty-hydra-define cat-github
  (:color teal :title (+with-icon "nf-cod-github" "GitHub"))
  ("" ()))

(use-package igist
  :pretty-hydra
  (cat-github
   ("gist"
    (("i" #'igist-dispatch "gist")))))

(use-package consult-gh
  :custom
  (consult-gh-default-clone-directory "~/Developer/")
  :pretty-hydra
  (cat-github
   ("gh"
    (("h" #'consult-gh "consult gh"))))
  :config
  ;; Remember visited orgs and repos across sessions
  (+add-to-list-multi 'savehist-additional-variables
                      'consult-gh--known-orgs-list
                      'consult-gh--known-repos-list)
  ;; Enable default keybindings (e.g. for commenting on issues, prs, ...)
  (consult-gh-enable-default-keybindings))

(use-package consult-gh-transient
  :ensure consult-gh
  :commands consult-gh-transient
  :custom
  (consult-gh-default-interactive-command #'consult-gh-transient))

(use-package consult-gh-embark
  :delight
  :demand t
  :after consult-gh
  :config
  (consult-gh-embark-mode))

(use-package consult-gh-forge
  :delight
  :demand t
  :after consult-gh
  :config
  (consult-gh-forge-mode))
