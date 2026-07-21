;; -*- lexical-binding: t; -*-

(require 'package-vc)

(defconst cat-package-vc-skip-unchanged-spec
  '(package-vc-skip-unchanged
    :url "https://github.com/chuxubank/package-vc-skip-unchanged")
  "VC package specification for `package-vc-skip-unchanged'.")

(add-to-list 'package-vc-selected-packages
             cat-package-vc-skip-unchanged-spec)

(unless (package-installed-p 'package-vc-skip-unchanged)
  (package-vc-install cat-package-vc-skip-unchanged-spec))

(package-activate 'package-vc-skip-unchanged)
(require 'package-vc-skip-unchanged)

(package-vc-skip-unchanged-mode 1)

(provide 'cat-package-vc-skip-unchanged)
