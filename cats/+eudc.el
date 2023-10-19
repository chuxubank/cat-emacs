;; -*- lexical-binding: t; -*-

(setq eudc-options-file (expand-file-name "eudc-options" cat-etc-dir))

(with-eval-after-load "message"
  (define-key message-mode-map [(control ?c) (tab)] 'eudc-expand-try-all))
(with-eval-after-load "sendmail"
  (define-key mail-mode-map [(control ?c) (tab)] 'eudc-expand-try-all))

(setopt eudc-server-hotlist
        '(("" . bbdb)
          ("ldaps://ldap.gnu.org" . ldap)))
(setopt ldap-host-parameters-alist
        '(("ldaps://ldap.gnu.org"
           base "ou=people,dc=gnu,dc=org"
           binddn "gnu\\emacsuser"
           passwd ldap-password-read)))

(with-eval-after-load 'eudc
  (when IS-MAC
    (require 'eudcb-macos-contacts)
    (eudc-macos-contacts-set-server "localhost")))

(use-package bbdb
  :defer t
  :custom
  (bbdb-file (expand-file-name "bbdb" cat-org-directory))
  :config
  (bbdb-initialize))
