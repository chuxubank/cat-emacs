;; -*- lexical-binding: t; -*-

(use-package diagram-preview
  :straight (diagram-preview :host github :repo "natrys/diagram-preview")
  :hook (graphviz-dot-mode
         plantuml-mode
         mermaid-mode
         pikchr-mode
         d2-mode))

(defun +diagram-preview-get-url ()
  "Return the URL of the diagram preview of STRING."
  (concat (diagram-preview--api-endpoint)
          (base64url-encode-string (buffer-string) t)))
