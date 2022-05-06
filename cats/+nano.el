;; -*- lexical-binding: t; -*-

(use-package nano-theme
  :config
  (setq
   default-frame-alist (list '(internal-border-width . 24)
			     '(left-fringe . 0)
			     '(right-fringe . 0)
			     '(vertical-scroll-bars . nil)
			     '(fullscreen . maximized)))
  ;; Nicer glyphs for continuation and wrap
  (set-display-table-slot standard-display-table
			  'truncation (make-glyph-code ?… 'nano-faded))
  (set-display-table-slot standard-display-table
			  'wrap (make-glyph-code ?… 'nano-faded)))

(use-package nano-modeline
  :config
  (nano-modeline-mode))

(use-package nano-agenda
  :disabled
  :straight t)
