(use-package org-roam
  :commands #'org-roam-buffer-toggle
  :config
  (org-roam-setup))

(setq org-roam-v2-ack t
      org-roam-directory (file-truename "~/org-roam")
      org-roam-completion-everywhere t
      org-roam-link-auto-replace nil)

(define-key global-map (kbd "C-c r f") #'org-roam-node-find)
(define-key global-map (kbd "C-c r g") #'org-roam-graph)
(define-key global-map (kbd "C-c r c") #'org-roam-capture)
(define-key global-map (kbd "C-c r i") #'org-roam-node-insert)
