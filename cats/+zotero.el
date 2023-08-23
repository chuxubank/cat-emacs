;; -*- lexical-binding: t; -*-

(use-package zotero
  :defer t
  :config
  (setq oauth-nonce-function #'oauth-internal-make-nonce
        zotero-cache-file (concat cat-cache-dir "zotero")
        zotero-cache-enable-storage nil
        zotero-cache-storage-dir "~/Zotero/storage/"))

(autoload #'zotero-browser "zotero-browser" nil t)
