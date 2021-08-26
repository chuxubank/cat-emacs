(use-package telega
  :commands #'telega)

(setq
 telega-symbol-folder "📁"
 telega-symbol-reply "↩")

(when (featurep 'selectrum)
  (setq telega-completing-read-function #'selectrum-completing-read))

(define-key global-map (kbd "C-c a t") telega-prefix-map)

(when cat-alt-code-font
  (with-eval-after-load 'telega
    (set-face-attribute 'telega-entity-type-pre nil :family cat-alt-code-font)
    (set-face-attribute 'telega-entity-type-code nil :family 'cat-alt-code-font)
    (set-face-attribute 'telega-webpage-fixed nil :family 'cat-alt-code-font)))
