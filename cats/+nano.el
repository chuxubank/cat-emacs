;; -*- lexical-binding: t; -*-

(use-package nano
  :disabled
  :straight (nano-emacs :host github :repo "rougier/nano-emacs"))

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
  :custom
  (nano-modeline-position 'nano-modeline-footer)
  :config
  (add-hook 'prog-mode-hook            #'nano-modeline-prog-mode)
  (add-hook 'text-mode-hook            #'nano-modeline-text-mode)
  (add-hook 'org-mode-hook             #'nano-modeline-org-mode)
  (add-hook 'pdf-view-mode-hook        #'nano-modeline-pdf-mode)
  (add-hook 'mu4e-headers-mode-hook    #'nano-modeline-mu4e-headers-mode)
  (add-hook 'mu4e-view-mode-hook       #'nano-modeline-mu4e-message-mode)
  (add-hook 'elfeed-show-mode-hook     #'nano-modeline-elfeed-entry-mode)
  (add-hook 'elfeed-search-mode-hook   #'nano-modeline-elfeed-search-mode)
  (add-hook 'term-mode-hook            #'nano-modeline-term-mode)
  (add-hook 'xwidget-webkit-mode-hook  #'nano-modeline-xwidget-mode)
  (add-hook 'messages-buffer-mode-hook #'nano-modeline-message-mode)
  (add-hook 'org-capture-mode-hook     #'nano-modeline-org-capture-mode)
  (add-hook 'org-agenda-mode-hook      #'nano-modeline-org-agenda-mode)
  (nano-modeline-text-mode t))

(use-package nano-minibuffer
  :disabled
  :straight (nano-minibuffer :host github :repo "rougier/nano-minibuffer")
  :config
  (nano-minibuffer-mode))
