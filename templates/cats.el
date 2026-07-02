;;; cats.el --- Default Cat Emacs module list -*- lexical-binding: t; -*-

;;; Commentary:

;; Module declarations for Cat Emacs.

;;; Code:

(defconst cat-modules
  '(:base
    package
    select-package
    benchmark
    default
    env
    local
    keymap

    :system
    (:if (daemonp)
         daemon)
    (:if IS-MAC
         macos)

    :emacs
    diredx
    tramp
    eudc
    term
    file
    everywhere

    :ui
    ;; nano
    doom
    font
    autodark
    beauty
    windows
    workspace
    dashboard

    :editor
    meow
    avy
    smartparens
    rime
    ;; hydra

    :completion
    ;; company
    corfu
    vertico
    ;; orderless
    prescient
    which-key
    consult
    embark
    yasnippet

    :text
    org
    org-babel
    org-todo
    org-roam
    org-notes
    org-latex
    org-srs
    org-cv
    org-link
    bibcite
    latex
    cdlatex
    ;; zotero
    markdown

    :code
    format
    flycheck
    (:if EMACS29+
         treesit)
    (:if (not EMACS29+)
         tree-sitter)
    doc
    sideline
    ;; lsp
    ;; lsp-bridge
    eglot

    :ai
    copilot
    vibe

    :lang
    android
    applescript
    cmake
    csv
    dart
    go
    gradle
    graphql
    ;; java
    kotlin
    kdl
    lua
    makefile
    mermaid
    node
    plantuml
    poly
    protobuf
    python
    rust
    shell
    swift
    vb
    web
    yaml

    :tools
    rg
    magit
    git-misc
    ansible
    caddy
    docker
    compile

    :apps
    (:if (eq HOST_ENV 'aa)
         atlassian
         org-jira)
    beancount
    blog
    chezmoi
    diagram-preview
    elfeed
    github
    im
    language
    log
    mail
    map
    media
    music
    nov
    oj
    pass
    pdf
    toc
    undo)
  "Cat Emacs module declarations.")

(provide 'cats)

;;; cats.el ends here
