;; -*- lexical-binding: t; -*-

(pretty-hydra-define cat-mail
  (:color teal :title (+with-icon "nf-oct-mail" "Mail"))
  ("" ()))

(defcustom cat/mu4e-print-types
  '(("PDF"  . "pdf")
    ("OFD"  . "ofd")
    ("Word" . "docx"))
  "List of file types for printing in `cat/mu4e-action-print-by-type'.

The first character of NAME is used as the shortcut."
  :group 'mu4e-headers
  :type '(alist :key-type string :value-type function))

(defun cat/mu4e-print-type-read ()
  "Print attachments or URLs of a single type from the current message."
  (mu4e-read-option "Type: " cat/mu4e-print-types))

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
        ("w" . #'mu4e-copy-thing-at-point))
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
                          (:maildir       .   20)
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
                              :receives temp))
  (+add-to-list-multi 'mu4e-view-actions
                      '("print" . cat/mu4e-action-print-by-type))
  (+add-to-list-multi 'mu4e-headers-actions
                      '("print" . cat/mu4e-action-print-by-type))
  (+add-to-list-multi 'mu4e-marks
                      '(print
                        :char ("p" . "󰐪")
                        :prompt "print"
                        :ask-target cat/mu4e-print-type-read
                        :action (lambda (docid msg type)
                                  (cat/mu4e-action-print-by-type msg type)
                                  (mu4e-action-retag-message msg "+printed"))))
  (+add-to-list-multi 'mu4e-action-tags-completion-list
                      "printed")
  (mu4e~headers-defun-mark-for print)
  (advice-add 'mu4e-action-retag-message :after #'cat/mu4e-retag-message-move))

(use-package file-url-extractor
  :ensure nil
  :commands file-url-extractor-get-all)

(defun cat/mu4e-retag-message-move (msg &rest _args)
  "After retagging MSG, move it to its current maildir on the server."
  (let ((docid (mu4e-message-field msg :docid))
        (maildir (mu4e-message-field msg :maildir)))
    (when (and docid maildir)
      (mu4e--server-move docid maildir))))

(defun cat/mu4e-view-message-files-and-urls (msg file-ext)
  "Return a plist with :local-files and :urls from MSG filtered by FILE-EXT.
Local attachments are saved via `mu4e--view-mime-part-to-temp-file'.
Remote URLs are kept as-is."
  (with-temp-buffer
    (insert-file-contents-literally (mu4e-message-readable-path msg) nil nil nil t)
    (let ((gnus-inhibit-mime-unbuttonizing nil)
          (gnus-unbuttonized-mime-types '(".*/.*"))
          (mu4e-view-fields '(:from :to :cc :subject :date))
          local-files urls)
      (mu4e--view-render-buffer msg)
      (setq urls (file-url-extractor-get-all (gnus-collect-urls) file-ext))
      (dolist (part (mu4e-view-mime-parts))
        (let ((handle (plist-get part :handle))
              (fname (plist-get part :filename)))
          (when (and handle fname
                     (plist-get part :attachment-like)
                     (string-match-p (concat "\\." (regexp-quote file-ext) "\\'") fname))
            (push (mu4e--view-mime-part-to-temp-file handle) local-files))))
      (list :local-files (nreverse local-files)
            :urls (nreverse urls)))))

(defun cat/mu4e-action-print-by-type (&optional msg file-type)
  "Print attachments or URLs of a single FILE-TYPE from the MSG.
User selects type first then choose one if multiple files."
  (interactive)
  (let* ((file-type (or file-type (cat/mu4e-print-type-read)))
         (files-and-urls (cat/mu4e-view-message-files-and-urls msg file-type))
         (local-files (plist-get files-and-urls :local-files))
         (urls (plist-get files-and-urls :urls))
         (candidates
          (append
           (mapcar (lambda (f)
                     (cons (format "LOCAL: %s" (file-name-nondirectory f))
                           (list :type 'local :path f)))
                   local-files)
           (mapcar (lambda (u)
                     (cons (format "URL: %s" u)
                           (list :type 'url :url u)))
                   urls))))
    (if (null candidates)
        (user-error "No %s files found" file-type)
      (let* ((chosen-entry
              (cdr
               (if (= (length candidates) 1)
                   (car candidates)
                 (assoc (completing-read
                         (format "Select %s to print: " file-type)
                         (mapcar #'car candidates) nil t)
                        candidates)))))
        (pcase (plist-get chosen-entry :type)
          ('local
           (start-process-shell-command "lpr-local" nil
                                        (format "%s %s"
                                                lpr-command
                                                (shell-quote-argument (plist-get chosen-entry :path)))))
          ('url
           (start-process-shell-command "lpr-remote" nil
                                        (format "curl -sL %s | %s"
                                                (shell-quote-argument (plist-get chosen-entry :url))
                                                lpr-command))))
        (message "Sent %s to printer"
                 (if (eq (plist-get chosen-entry :type) 'local)
                     (plist-get chosen-entry :path)
                   (plist-get chosen-entry :url)))))))

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
