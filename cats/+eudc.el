;; -*- lexical-binding: t; -*-

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
  :custom
  (bbdb-file (expand-file-name "bbdb.gpg" cat-org-directory))
  :config
  (bbdb-initialize))
