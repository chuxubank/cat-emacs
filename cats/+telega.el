(use-package telega
  :commands #'telega)

(setq
 telega-symbol-folder "📁"
 telega-symbol-reply "↩"
 telega-filter-default '(custom "Focus"))

(when (featurep 'selectrum)
  (setq telega-completing-read-function #'selectrum-completing-read))

(define-key global-map (kbd "C-c a t") telega-prefix-map)

(with-eval-after-load 'telega
  (when cat-alt-code-font
    (set-face-attribute 'telega-entity-type-pre nil :family cat-alt-code-font)
    (set-face-attribute 'telega-entity-type-code nil :family cat-alt-code-font)
    (set-face-attribute 'telega-webpage-fixed nil :family cat-alt-code-font))
  (add-to-list
   'telega-filters-custom
   '("Focus" . (not (folder "NSFW" "Proxy")))))
