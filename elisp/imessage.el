;; iMessage.el --- Control iMessage
;; Copyright 2016 Chad Sahlhoff
;;
;; Author: Chad Sahlhoff <chad@sahlhoff.com>
;; Maintainer: Chad Sahlhoff <chad@sahlhoff.com>
;; Keywords: vertico iMessage
;; URL: https://github.com/sahlhoff/imessage
;; Created: 1st May 2016
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1") (s "1.0.0"))

;;; Commentary:
;;
;; An iMessage interface for macOS
;;
;; Only supports macOS and iMessage.
;; Now uses `completing-read', integrates nicely with `vertico'.

;;; Code:

(require 's)

(defvar imessage-disable-service-check t
  "Sometimes a message won't send if the service is passed in. Disable this if you run into issues.")

(defun imessage-call-osascript (cmd)
  "Run AppleScript CMD inside 'tell application \"Messages\"' context."
  (let* ((script (format "osascript -e 'tell application \"Messages\" %s'" cmd))
         (result (shell-command-to-string script)))
    (message "OSAScript: %s\n=> %s" script result)
    result))

(defun imessage-get-buddies-names ()
  "Get list of iMessage buddy names via AppleScript."
  (imessage-parse-blob
   (imessage-call-osascript "to get name of buddies")))

(defun imessage-get-service-buddy (buddy)
  "Get the service name of BUDDY."
  (unless imessage-disable-service-check
    (string-trim
     (imessage-call-osascript (format "to get name of service of buddy %S" buddy)))))

(defun imessage-get-message ()
  "Prompt user to enter message content."
  (format "%S" (read-string "Message to send: ")))

(defun imessage-send-buddy-message (buddy)
  "Send message to BUDDY, including service if enabled."
  (let* ((service (imessage-get-service-buddy buddy))
         (message-str (imessage-get-message))
         (cmd (format "to send %s to buddy %S" message-str buddy)))
    (when service
      (setq cmd (concat cmd (format " of service %S" service))))
    (imessage-call-osascript cmd)))

(defun imessage-parse-blob (blob)
  "Split AppleScript BLOB into a list of buddy names."
  (mapcar #'string-trim (split-string blob ",")))

;;;###autoload
(defun imessage ()
  "Prompt for a buddy using `completing-read`, then send them a message."
  (interactive)
  (let* ((buddies (imessage-get-buddies-names))
         (selected (completing-read "Send message to: " buddies nil t)))
    (imessage-send-buddy-message selected)))

(provide 'imessage)
;;; iMessage.el ends here
