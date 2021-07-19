(use-package telega
  :commands #'telega)

(setq telega-symbol-folder "📁"
      telega-symbol-reply "↫")

(when (featurep 'selectrum)
  (setq telega-completing-read-function #'selectrum-completing-read))


(define-key global-map (kbd "C-c a t") #'telega)
