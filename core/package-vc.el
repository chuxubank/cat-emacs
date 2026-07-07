;; -*- lexical-binding: t; -*-

(require 'subr-x)

(defun package-vc-skip-if-same-hash (orig-fn pkg-desc)
  "Skip `package-vc-upgrade' if PKG-DESC is already at the latest commit.
Only applies to Git VC packages; otherwise, run ORIG-FN."
  (require 'vc)
  (let* ((pkg-dir (package-desc-dir pkg-desc))
         (backend (vc-responsible-backend pkg-dir)))
    (if (not (eq backend 'Git))
        (funcall orig-fn pkg-desc)
      (require 'vc-git)
      (let ((default-directory pkg-dir))
        (condition-case nil
            (let* ((upstream (string-trim
                              (with-output-to-string
                                (vc-git-command standard-output nil nil
                                                "rev-parse" "--abbrev-ref" "@{u}")))))
              (if (string-empty-p upstream)
                  (funcall orig-fn pkg-desc)
                (vc-git-command t 0 nil "fetch" "--quiet")
                (let* ((local (string-trim
                               (with-output-to-string
                                 (vc-git-command standard-output nil nil "rev-parse" "HEAD"))))
                       (remote (string-trim
                                (with-output-to-string
                                  (vc-git-command standard-output nil nil "rev-parse" "@{u}")))))
                  (if (string= local remote)
                      (message "Package %s already up-to-date" (package-desc-name pkg-desc))
                    (funcall orig-fn pkg-desc)))))
          (error
           (funcall orig-fn pkg-desc)))))))

(with-eval-after-load 'package-vc
  (advice-add 'package-vc-upgrade :around #'package-vc-skip-if-same-hash))

(provide 'cat-package-vc)
