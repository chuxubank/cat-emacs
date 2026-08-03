;;; custom.el --- Default Cat Emacs custom settings -*- lexical-binding: t; -*-

;;; Commentary:

;; This file is the fallback Custom file for Cat Emacs.  User customizations
;; should live in ~/.config/cat-emacs/custom.el.

;;; Code:

(custom-set-variables
 '(face-font-family-alternatives
   '(("Sans Serif" "Iosevka Aile" "Inter" "Avenir Next" "DejaVu Sans")
     ("Serif" "DejaVu Serif" "Roboto Serif" "Charter" "Georgia")
     ("Monospace" "Iosevka Term" "Iosevka" "Menlo"
      "DejaVu Sans Mono")
     ("Monospace Sans Serif" "Maple Mono" "JetBrains Mono"
      "Cascadia Code" "Fira Code" "SF Mono" "IBM Plex Mono"
      "Roboto Mono" "Menlo" "DejaVu Sans Mono")
     ("Monospace Serif" "Iosevka Etoile" "Roboto Slab" "Charter"
      "DejaVu Serif")))
 '(gptel-model-updater-backends
   '(gptel--gemini gptel--llama gptel--mlx gptel--ollama
                   gptel--openrouter))
 '(gptel-model-updater-external-targets
   '((gptel-magit-backend gptel-magit-model "GPTel-Magit"
                          ("OpenRouter:openai/gpt-oss-120b:free"))
     (gptel-forge-prs-backend gptel-forge-prs-model "GPTel-Forge-Prs"
                              ("OpenRouter:openai/gpt-oss-120b:free"))))
 '(gptel-model-updater-models '("OpenRouter:auto"))
 '(use-short-answers t)
 '(package-native-compile t)
 '(system-packages-use-sudo nil))

;;; custom.el ends here
