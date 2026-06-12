;;; cats.example.el --- Example Cat Emacs module list -*- lexical-binding: t; -*-

;;; Commentary:

;; Module declarations for Cat Emacs.

;;; Code:

(defconst cat-modules
  '(:config
    package
    utils
    benchmark
    default
    env
    local

    :enhance
    diredx
    tramp
    eudc
    term
    file

    :ui
    ;; nano
    doom
    font
    autodark
    beauty
    windows
    workspace
    dashboard

    :daemon
    (:if (daemonp)
         daemon)

    :os
    (:if IS-MAC
         macos)

    :editor
    meow
    avy
    smartparens
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

    :search
    rg

    :git
    magit
    git-misc

    :org
    org
    org-babel
    org-todo
    org-roam
    org-notes
    org-latex
    org-srs
    org-cv
    org-link

    :latex
    bibcite
    latex
    cdlatex
    ;; zotero

    :markdown
    markdown

    :csv
    csv

    :input
    rime

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

    :program
    android
    applescript
    cmake
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
    ansible
    caddy
    docker
    compile

    :plugins
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
    undo

    :keymap
    keymap)
  "Cat Emacs module declarations.")

(provide 'cats.example)

;;; cats.example.el ends here
