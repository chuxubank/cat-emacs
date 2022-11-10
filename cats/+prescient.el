;; -*- lexical-binding: t; -*-

(when (featurep 'company)
  (use-package company-prescient)
  (company-prescient-mode 1))

(when (featurep 'selectrum)
  (use-package selectrum-prescient)
  (selectrum-prescient-mode 1))

(when (featurep 'vertico)
  ;; Enable the completion style.
  (setq completion-styles '(prescient basic)
	completion-category-overrides '((file (styles basic-remote partial-completion))))

  ;; Use `prescient-completion-sort' after filtering.
  (setq vertico-sort-function #'prescient-completion-sort)

  (defun vertico-prescient-remember ()
    "Remember the chosen candidate with Prescient."
    (when (>= vertico--index 0)
      (prescient-remember
       (substring-no-properties
	(nth vertico--index vertico--candidates)))))
  (advice-add #'vertico-insert :after #'vertico-prescient-remember))

(setq prescient-save-file (expand-file-name "prescient-save.el" cat-cache-dir))
(prescient-persist-mode 1)
