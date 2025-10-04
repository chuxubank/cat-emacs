;; -*- lexical-binding: t; -*-

(pretty-hydra-define cat-mail
  (:color teal :title (+with-icon "nf-oct-mail" "Mail"))
  ("" ()))

(use-package mu4e
  :commands #'mu4e
  :ensure-system-package
  (mu)
  (mbsync . isync)
  :ensure nil
  :bind
  (:map mu4e-main-mode-map
        ("q" . #'bury-buffer)
        ("Q" . #'mu4e-quit)
        ("u" . #'mu4e-update-index))
  (:map mu4e-view-mode-map
        ("M-w" . #'mu4e-copy-thing-at-point))
  :custom
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-change-filenames-when-moving t)
  (mu4e-trash-without-flag t)
  (mu4e-update-interval 300)
  (mu4e-use-fancy-chars t)
  (mu4e-sent-messages-behavior 'delete)
  (mu4e-read-option-use-builtin nil)
  (mu4e-completing-read-function 'completing-read)
  (mu4e-attachment-dir (expand-file-name "~/Downloads/"))
  (mu4e-headers-fields '( (:human-date    .   12)
                          (:labels        .    8)
                          (:flags         .    6)
                          (:tags          .   20)
                          (:maildir       .   10)
                          (:mailing-list  .   10)
                          (:from          .   22)
                          (:subject       .   nil)))
  (mu4e-view-fields '(:from
                      :to
                      :cc
                      :subject
                      :labels
                      :flags
                      :date
                      :maildir
                      :mailing-list
                      :tags))
  (mu4e-headers-actions '(("capture message"  . mu4e-action-capture-message)
                          ("retag message" . mu4e-action-retag-message)
                          ("browse online archive" . mu4e-action-browse-list-archive)
                          ("show this thread" . mu4e-action-show-thread)))
  (mu4e-action-tags-completion-list '("printed"))
  :pretty-hydra
  (cat-mail
   ("Mu4e"
    (("m" #'mu4e "mu4e")
     ("k" #'mu4e-quit "quit")
     ("u" #'mu4e-update-index "update index")
     ("U" #'mu4e-update-mail-and-index "update mail and index"))))
  :config
  (+add-to-list-multi 'mu4e-view-mime-part-actions
                      '(:name "print"
                              :handler (lambda (file)
                                         (call-process-shell-command lpr-command file))
                              :receives temp)
                      '(:name "url-print"
                              :handler cat/mu4e--print-pdf-url
                              :receives pipe))
  (+add-to-list-multi 'mu4e-marks
                      '(tag
                        :char ("t" . "")
                        :prompt "tag"
                        :ask-target (lambda () (read-string "Add tag: "))
                        :action (lambda (docid msg target)
                                  (mu4e-action-retag-message msg target)
                                  (mu4e--server-move docid
                                                     (mu4e-msg-field msg :maildir)))))
  (mu4e~headers-defun-mark-for tag))

(defun cat/mu4e--print-pdf-url (str)
  "Find PDF URLs in the STR, detect PDFs, download and send to printer."
  (let* ((urls (let (results)
                 (let ((pos 0))
                   (while (string-match goto-address-url-regexp str pos)
                     (push (match-string 0 str) results)
                     (setq pos (match-end 0))))
                 (nreverse results)))
         (pdf-urls-fast (cl-remove-if-not
                         (lambda (url)
                           (string-match-p "\\.pdf" url))
                         urls))
         (pdf-urls-checked (append
                            pdf-urls-fast
                            (cl-remove-if-not
                             (lambda (url)
                               (with-temp-buffer
                                 (when (eq 0 (call-process "curl" nil t nil "-sI" "-L" url))
                                   (goto-char (point-min))
                                   (re-search-forward "Content-Type: *application/pdf" nil t))))
                             (cl-set-difference urls pdf-urls-fast :test #'equal))))
         (url (cl-case (length pdf-urls-checked)
                (0 (user-error "No PDF URLs detected in this message"))
                (1 (car pdf-urls-checked))
                (t (completing-read "Print PDF URL: " pdf-urls-checked nil t))))
         (cmd (format "curl -sL %s | %s"
                      (shell-quote-argument url)
                      lpr-command)))
    (start-process-shell-command "mu4e-print-pdf" nil cmd)
    (message "Sent %s to printer via %s" url lpr-command)))

(defun cat/mu4e--update-mail-and-index-real-around (orig-fun run-in-background)
  "Temporarily set `mu4e-get-mail-command' to \"true\".
When run ORIG-FUN with RUN-IN-BACKGROUND not nil.

So we can take advantage of the brew service's schedule function."
  (if run-in-background
      (let ((mu4e-get-mail-command "true"))
        (funcall orig-fun run-in-background))
    (funcall orig-fun run-in-background)))

(advice-add 'mu4e--update-mail-and-index-real :around #'cat/mu4e--update-mail-and-index-real-around)

(use-package mu4e-column-faces
  :demand
  :after mu4e
  :config (mu4e-column-faces-mode))

(use-package mu4e-overview
  :pretty-hydra
  (cat-mail
   ("Mu4e"
    (("o" #'mu4e-overview "overview")))))

(use-package gnus
  :ensure nil
  :custom
  (gnus-select-method '(nntp "news.gmane.io"))
  :pretty-hydra
  (cat-mail
   ("Gnus"
    (("g" #'gnus "Gnus")))))
