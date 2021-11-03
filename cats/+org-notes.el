(use-package org-media-note
  :straight (org-media-note :host github :repo "yuchen-lea/org-media-note")
  :hook (org-mode .  org-media-note-mode))

(use-package org-noter
  :defer t
  :custom
  (org-noter-always-create-frame nil))

(use-package org-noter-pdftools
  :disabled
  :after org-noter
  :config
  ;; Add a function to ensure precise note is inserted
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freestyle-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))

  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c n m") #'org-media-note-hydra/body)
  (define-key org-mode-map (kbd "C-c n n") #'org-noter))
