;; -*- lexical-binding: t; -*-

(use-package cdlatex
  :delight
  (cdlatex-mode " ")
  :hook (org-mode . org-cdlatex-mode))

(setq cdlatex-env-alist
      '(("equation*" "\\begin{equation*}\n?\n\\end{equation*}" nil)
        ("cases" "\\begin{cases}\n? & ,\\\\\n & .\n\\end{cases}" nil)
        ("tikzpicture" "\\begin{tikzpicture}\n?\n\\end{tikzpicture}" nil)))

(setq cdlatex-command-alist
      '(
        ("equ*"       "Insert equation* env"
         "" cdlatex-environment ("equation*") t nil)
        ("cas"        "Insert cases env"
         "" cdlatex-environment ("cases") t nil)
        ("tikz"       "Insert tikzpicture env"
         "" cdlatex-environment ("tikzpicture") t nil)
        
        ("liml"         "Insert \\lim\\limits_{ \\to }"
         "\\lim\\limits_{? \\to }" cdlatex-position-cursor nil nil t)
        ))

(setq cdlatex-math-modify-alist
      '((?V "\\vb*" nil t nil nil)
        (?b "\\bm" nil t nil nil)))

(setq cdlatex-math-symbol-alist
      '((?L ("\\Lambda" "\\varLambda"))))
