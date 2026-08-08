;; -*- lexical-binding: t; -*-

(mode-transient-define-prefix cat-github ()
  :description (+with-icon "nf-cod-github" nil " GitHub"))

(use-package igist
  :transient
  (cat-github
   ["gist"
    ("i" "gist" igist-dispatch)]))

(use-package consult-gh
  :custom
  (consult-gh-default-clone-directory "~/Developer/")
  :transient
  (cat-github
   ["gh"
    ("h" "consult gh" consult-gh)])
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
