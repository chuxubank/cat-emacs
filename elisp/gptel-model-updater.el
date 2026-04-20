;;; gptel-model-updater.el --- Fetch and update models for GPTel backends -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Misaka

;; Author: pfcdx <github@pfcdx>
;; Maintainer: Misaka <chuxubank@qq.com>
;; Version: 0.2.0
;; Package-Requires: ((gptel))
;; Keywords: gptel, ai, llm
;; URL: https://github.com/chuxubank/cat-emacs/elisp/gptel-model-updater.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A package to fetch and update models for GPTel backends from various providers.
;; Supports OpenAI-compatible APIs, Ollama, and Gemini.

;;; Code:

(require 'url)
(require 'json)
(require 'cl-lib)
(require 'gptel)

(defgroup gptel-model-updater nil
  "GPTel model updater settings."
  :group 'gptel)

(defcustom gptel-model-updater-timeout 30
  "Timeout for API requests in seconds."
  :type 'number
  :group 'gptel-model-updater)

(defun gptel-model-updater--resolve-key (key)
  "Recursively resolve KEY until it yields a string or nil."
  (cond
   ((stringp key) key)
   ((functionp key) (gptel-model-updater--resolve-key (funcall key)))
   ((and (symbolp key) (fboundp key)) (gptel-model-updater--resolve-key (funcall key)))
   ((and (symbolp key) (boundp key)) (gptel-model-updater--resolve-key (symbol-value key)))
   (t nil)))

(defun gptel-model-updater--detect-provider (backend)
  "Detect provider type for BACKEND struct.
Returns one of `openai', `gemini', or `ollama'."
  (cond
   ((cl-typep backend 'gptel-gemini) 'gemini)
   ((cl-typep backend 'gptel-ollama) 'ollama)
   ((cl-typep backend 'gptel-openai) 'openai)
   (t 'openai)))

(defun gptel-model-updater--build-url (backend provider-type)
  "Build the models list URL for BACKEND given PROVIDER-TYPE."
  (let ((host (gptel-backend-host backend))
        (protocol (or (gptel-backend-protocol backend) "https")))
    (pcase provider-type
      ('gemini
       (let ((api-key (gptel-model-updater--resolve-key (gptel-backend-key backend))))
         (format "%s://%s/v1beta/models?key=%s&pageSize=1000" protocol host (or api-key ""))))
      ('ollama
       (format "%s://%s/api/tags" protocol host))
      (_
       (format "%s://%s/v1/models" protocol host)))))

(defun gptel-model-updater--build-headers (provider-type api-key)
  "Build request headers for PROVIDER-TYPE with API-KEY."
  (append '(("Content-Type" . "application/json"))
          (pcase provider-type
            ('openai
             (when api-key
               `(("Authorization" . ,(concat "Bearer " api-key)))))
            ;; Gemini uses query param for key, no auth header needed
            (_ nil))))

(defun gptel-model-updater--fetch-models (backend-name provider-type url headers callback)
  "Fetch models from URL and call CALLBACK with the result.
BACKEND-NAME is used for messages.
PROVIDER-TYPE is `openai', `ollama', or `gemini'.
URL is the endpoint to fetch from.
HEADERS is the request headers alist.
CALLBACK is called with (success raw-data error-message)."
  (let* ((url-request-method "GET")
         (url-request-timeout gptel-model-updater-timeout)
         (url-request-extra-headers headers))
    (message "GPTel-Model-Updater: Contacting %s..." backend-name)
    (url-retrieve
     url
     (lambda (status)
       (let ((http-code (url-http-parse-response)))
         (cond
          ((plist-get status :error)
           (funcall callback nil nil
                    (format "Connection failed: %s" (plist-get status :error))))
          ((not (eq http-code 200))
           (funcall callback nil nil (format "HTTP %d" http-code)))
          (t
           (goto-char (point-min))
           (re-search-forward "^$" nil 'move)
           (let* ((json-object-type 'alist)
                  (json-key-type 'symbol)
                  (response (condition-case nil (json-read) (error nil))))
             (if (not response)
                 (funcall callback nil nil "Failed to parse JSON")
               (let ((raw-data (pcase provider-type
                                 ('gemini (alist-get 'models response))
                                 ('ollama (alist-get 'models response))
                                 (_ (alist-get 'data response)))))
                 (when (vectorp raw-data)
                   (setq raw-data (append raw-data nil)))
                 (funcall callback t raw-data nil))))))))
     nil t)))

(defun gptel-model-updater--parse-models (raw-data provider-type)
  "Parse RAW-DATA from PROVIDER-TYPE into a list of model symbols."
  (let ((models nil))
    (dolist (m raw-data)
      (let ((id (pcase provider-type
                  ('gemini
                   ;; Gemini returns "models/gemini-2.5-pro", strip prefix
                   (let ((name (or (alist-get 'name m) "")))
                     (if (string-prefix-p "models/" name)
                         (substring name 7)
                       name)))
                  (_
                   (or (alist-get 'id m)
                       (alist-get 'name m))))))
        (when (and id (not (string-empty-p id)))
          (push (intern id) models))))
    (when models
      (setq models
            (sort (delete-dups models)
                  (lambda (a b) (string< (symbol-name a) (symbol-name b))))))
    models))

(defun gptel-model-updater--get-backends ()
  "Get list of available backend names.
Iterates over `gptel--backends' (a list of symbols pointing to
backend structs) and returns their name strings."
  (cl-loop for sym in gptel--backends
           when (and (symbolp sym) (boundp sym))
           collect (gptel-backend-name (symbol-value sym))))

;;;###autoload
(defun gptel-model-updater-update-backend (backend-name &optional provider-type url)
  "Update models for BACKEND-NAME.
PROVIDER-TYPE can be `openai', `ollama', or `gemini'.
If nil, it is auto-detected from the backend struct type.
URL overrides the default endpoint."
  (interactive
   (let ((backends (gptel-model-updater--get-backends)))
     (list (completing-read "Backend: " backends nil t))))
  (let* ((backend (gptel-get-backend backend-name))
         (provider (or provider-type (gptel-model-updater--detect-provider backend)))
         (api-key (gptel-model-updater--resolve-key (gptel-backend-key backend)))
         (fetch-url (or url (gptel-model-updater--build-url backend provider)))
         (headers (gptel-model-updater--build-headers provider api-key)))

    (when (and (eq provider 'openai) (not api-key))
      (user-error "GPTel-Model-Updater: No API key found for %s" backend-name))

    (gptel-model-updater--fetch-models
     backend-name provider fetch-url headers
     (lambda (success raw-data error-msg)
       (if (not success)
           (message "GPTel-Model-Updater Error: %s (%s)" backend-name error-msg)
         (let ((new-models (gptel-model-updater--parse-models raw-data provider)))
           (if (not new-models)
               (message "GPTel-Model-Updater: No models found for %s" backend-name)
             (setf (gptel-backend-models backend) new-models)
             (message "GPTel-Model-Updater: Updated %s with %d models"
                      backend-name (length new-models)))))))))

;;;###autoload
(defun gptel-model-updater-update-all ()
  "Update models for all configured GPTel backends."
  (interactive)
  (dolist (sym gptel--backends)
    (when (and (symbolp sym) (boundp sym))
      (let ((name (gptel-backend-name (symbol-value sym))))
        (condition-case err
            (gptel-model-updater-update-backend name)
          (error (message "GPTel-Model-Updater: Failed to update %s: %s" name err)))))))

(provide 'gptel-model-updater)
;;; gptel-model-updater.el ends here
