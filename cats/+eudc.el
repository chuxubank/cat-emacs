;; -*- lexical-binding: t; -*-

(pretty-hydra-define cat-eudc
  (:color teal :title (+with-icon "nf-md-contacts" "EUDC"))
  ("Action"
   (("t" #'eudc-expand-try-all "expand")
    ("f" #'eudc-query-form "query form")
    ("p" #'eudc-get-phone "get phone")
    ("e" #'eudc-get-email "get email")
    ("l" #'eudc-get-attribute-list "get list"))))

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
    (add-to-list 'eudc-server-hotlist '("localhost" . macos-contacts))))

(use-package bbdb
  :custom
  (bbdb-file (expand-file-name "bbdb.gpg" cat-org-directory))
  :config
  (bbdb-initialize)
  :pretty-hydra
  (cat-eudc
   ("BBDB"
    (("b" #'bbdb "bbdb")
     ("a" #'bbdb-create "create")))))
