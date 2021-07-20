(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-check-signature nil)

(require 'use-package-ensure)
(setq use-package-always-ensure t)
