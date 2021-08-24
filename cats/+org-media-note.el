(use-package org-media-note
  :straight (org-media-note :host github :repo "yuchen-lea/org-media-note")
  :hook (org-mode .  org-media-note-mode))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c m") #'org-media-note-hydra/body))
