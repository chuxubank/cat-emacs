;; -*- lexical-binding: t; -*-

(use-package pass
  :ensure-system-package
  (zbarimg . zbar)
  (qrencode . qrencode))

(use-package password-store-menu
  :commands #'password-store-menu)

(use-package password-store
  :custom
  (password-store-password-length 16)
  :config
  (defun password-store-generate (entry &optional password-length)
    "Generate a new password for ENTRY with PASSWORD-LENGTH.

Default PASSWORD-LENGTH is `password-store-password-length'."
    (interactive (list (password-store--completing-read)
                       (when current-prefix-arg
                         (abs (prefix-numeric-value current-prefix-arg)))))
    ;; A message with the output of the command is not printed
    ;; because the output contains the password.
    ;; Generate password in-place if entry file exists.
    (password-store--run-generate
     entry
     (or password-length password-store-password-length)
     (if (file-exists-p (password-store--entry-to-file entry)) t nil))
    nil)
  (defun password-store--run-generate (entry password-length &optional in-place no-symbols)
    (password-store--run "generate"
                         (if in-place "--in-place"
                           "--force")
                         (if no-symbols "--no-symbols")
                         entry
                         (number-to-string password-length))))

(defun cat-kill-gpg-agent ()
  (interactive)
  (shell-command "gpgconf --kill gpg-agent"))
