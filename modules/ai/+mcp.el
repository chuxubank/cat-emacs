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

(use-package elisp-dev-mcp
  :hook
  (cat-idle-preload . elisp-dev-mcp-enable))

(use-package org-mcp
  :commands org-mcp-enable
  :hook
  (cat-idle-preload . org-mcp-enable)
  :custom
  (org-mcp-allowed-files
   (directory-files-recursively cat-org-directory "\\.org\\'")))

(provide '+mcp)
;;; +mcp.el ends here
