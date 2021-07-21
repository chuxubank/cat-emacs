(use-package org-roam
  :defer t)

(setq org-roam-v2-ack t
      org-roam-directory (file-truename "~/org-roam")
      org-roam-completion-everywhere t
      org-roam-link-auto-replace nil
      org-agenda-text-search-extra-files (directory-files-recursively org-roam-directory "\\.org$"))

(define-key global-map (kbd "C-c r f") #'org-roam-node-find)
(define-key global-map (kbd "C-c r g") #'org-roam-graph)
(define-key global-map (kbd "C-c r c") #'org-roam-capture)
(define-key global-map (kbd "C-c r i") #'org-roam-node-insert)

(with-eval-after-load 'org-roam
  (define-key global-map (kbd "C-c r r") #'org-roam-buffer-toggle))
