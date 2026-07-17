;;; poly-any-go-template-test.el --- Tests for poly-any-go-template -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'poly-any-go-template)

(ert-deftest poly-any-go-template-detects-host-mode ()
  (let ((auto-mode-alist '(("\\.host\\'" . text-mode))))
    (should (eq (poly-any-go-template--get-major-mode-for-file
                 "/tmp/deployment.host")
                'text-mode))))

(ert-deftest poly-any-go-template-uses-go-template-inner-mode ()
  (should (eq (eieio-oref poly-go-template-innermode 'mode)
              'go-template-ts-mode)))

(ert-deftest poly-any-go-template-double-extension-takes-precedence ()
  (let ((poly-entry (cl-position '("\\.[^./]+\\.gotmpl\\'"
                                  . poly-any-go-template-mode)
                                 auto-mode-alist :test #'equal))
        (plain-entry (cl-position '("\\.gotmpl\\'" . go-template-ts-mode)
                                  auto-mode-alist :test #'equal)))
    (should poly-entry)
    (should plain-entry)
    (should (< poly-entry plain-entry))))

(ert-deftest poly-any-go-template-fontifies-host-and-inner ()
  (skip-unless (and (fboundp 'yaml-ts-mode) (treesit-ready-p 'yaml)
                    (treesit-ready-p 'gotmpl)))
  (with-temp-buffer
    (setq buffer-file-name "/tmp/deployment.yaml.gotmpl")
    (insert "name: {{ printf \"%s\" .Release.Name }}\n")
    (poly-any-go-template-mode)
    (let ((poly-lock-allow-background-adjustment nil))
      (pm-map-over-spans
       (lambda (_span)
         ;; Batch Emacs does not enable font lock in indirect buffers.
         (setq font-lock-mode t)
         (setq-local poly-lock-allow-fontification t)
         (poly-lock-mode t)))
      (font-lock-ensure))
    (goto-char (point-min))
    (search-forward "name")
    (should (eq (get-text-property (1- (point)) 'face)
                'font-lock-property-use-face))
    (search-forward "printf")
    (should (eq (get-text-property (1- (point)) 'face)
                'font-lock-builtin-face))))

(provide 'poly-any-go-template-test)
;;; poly-any-go-template-test.el ends here
