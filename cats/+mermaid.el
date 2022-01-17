;; -*- lexical-binding: t; -*-

(use-package mermaid-mode
  :mode "\\.mmd\\'"
  :config
  (setq mermaid-tmp-dir (expand-file-name "mermaid" cat-cache-dir)
        mermaid-output-format ".svg"
	;; FIXME: Add useMaxWidth config to fix svg display in org file.
        mermaid-flags (concat "-b transparent -t dark"))
  ;; WATCH: https://github.com/abrochard/mermaid-mode/pull/4
  (defun org-babel-execute:mermaid (body params)
    "Execute command with BODY and PARAMS from src block."
    (let* ((out-file (or (cdr (assoc :file params))
                         (org-babel-temp-file "mermaid-" mermaid-output-format)))
           (temp-file (org-babel-temp-file "mermaid-"))
           (cmd (concat (shell-quote-argument mermaid-mmdc-location)
                        " -o " (org-babel-process-file-name out-file)
                        " -i " temp-file
                        " " mermaid-flags)))
      (with-temp-file temp-file (insert body))
      (org-babel-eval cmd "")
      (unless (cdr (assq :file params))
        out-file))))
