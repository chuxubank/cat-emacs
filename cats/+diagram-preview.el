;; -*- lexical-binding: t; -*-

(use-package diagram-preview
  :vc (:url "https://github.com/natrys/diagram-preview")
  :delight " "
  :hook (graphviz-dot-mode
         plantuml-mode
         mermaid-mode
         pikchr-mode
         d2-mode))

(defun +diagram-preview-get-url ()
  "Return the URL of the diagram preview of STRING."
  (concat (diagram-preview--api-endpoint)
          (base64url-encode-string (buffer-string) t)))
