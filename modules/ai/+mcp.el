;; -*- lexical-binding: t; -*-

(use-package mcp-server
  :cat server
  :vc (:url "https://github.com/rhblind/emacs-mcp-server")
  :hook
  (cat-idle-preload . mcp-server-start-unix)
  :custom
  (mcp-server-socket-directory cat-cache-dir))

(use-package mcp-server-lib
  :hook
  (cat-idle-preload . mcp-server-lib-start)
  :custom
  (mcp-server-lib-install-directory cat-etc-dir))

(use-package elisp-dev-mcp)

(use-package org-mcp)
