;; -*- lexical-binding: t; -*-

(use-package mermaid-mode
  :mode "\\.mmd\\'"
  :config
  (setq mermaid-tmp-dir (concat cat-cache-dir "mermaid/")
        mermaid-output-format ".svg"
        mermaid-flags "-b transparent -t dark"))
