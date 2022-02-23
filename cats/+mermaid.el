;; -*- lexical-binding: t; -*-

(use-package mermaid-mode
  :mode "\\.mmd\\'"
  :config
  (setq mermaid-tmp-dir (concat cat-cache-dir "mermaid/")
        mermaid-output-format ".pdf"
        mermaid-flags "-b transparent -f"))
