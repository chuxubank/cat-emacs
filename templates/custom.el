;;; custom.el --- Default Cat Emacs custom settings -*- lexical-binding: t; -*-

;;; Commentary:

;; This file is the fallback Custom file for Cat Emacs.  User customizations
;; should live in ~/.config/cat-emacs/custom.el.

;;; Code:

(custom-set-variables
 '(face-font-family-alternatives
   '(("Sans Serif UI" "Inter" "Avenir Next" "DejaVu Sans")
     ("Serif" "Charter" "Roboto Serif" "DejaVu Serif" "Georgia")
     ("Monospace Narrow" "Iosevka" "Iosevka Term")
     ("Monospace Code" "Maple Mono" "Source Code Pro")
     ("Monospace Sans Serif" "Roboto Mono" "DejaVu Sans Mono")
     ("Slab Serif" "Roboto Slab" "American Typewriter")
     ("Quasi Proportional" "Iosevka Etoile" "Iosevka Aile")
     ("CJK Serif" "Songti SC" "LXGW WenKai" "Noto Serif CJK SC"
      "Source Han Serif SC")
     ("CJK Sans Serif" "PingFang SC" "Hiragino Sans GB"
      "Noto Sans CJK SC" "Source Han Sans SC" "Microsoft YaHei")
     ("CJK Monospace" "LXGW WenKai Mono" "Sarasa Mono SC")))
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
