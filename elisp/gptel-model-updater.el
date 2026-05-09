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

(defcustom gptel-model-updater-after-update-hook #'gptel-model-updater-select-backend-models
  "Hook run after a backend's models are updated successfully.
Each function is called with BACKEND-NAME, BACKEND, and MODELS."
  :type 'hook
  :group 'gptel-model-updater)

(defcustom gptel-model-updater-backends nil
  "GPTel backend symbols managed by `gptel-model-updater'."
  :type '(repeat symbol)
  :group 'gptel-model-updater)

(defcustom gptel-model-updater-external-targets nil
  "External backend/model variable pairs set by `gptel-model-updater'.
Each item is (BACKEND-VARIABLE MODEL-VARIABLE DISPLAY-NAME).  These
targets are selected and set when
`gptel-model-updater-select-backend-models' is called with external
targets enabled, such as via a C-u prefix interactively."
  :type '(repeat (list symbol symbol string))
  :group 'gptel-model-updater)

(defun gptel-model-updater--backends ()
  "Return backend symbols managed by `gptel-model-updater'."
  gptel-model-updater-backends)

(defun gptel-model-updater--detect-provider (backend)
  "Detect provider type for BACKEND struct.
Returns one of `openai', `gemini', or `ollama'."
  (cond
   ((cl-typep backend 'gptel-gemini) 'gemini)
   ((cl-typep backend 'gptel-ollama) 'ollama)
   ((cl-typep backend 'gptel-openai) 'openai)
   (t 'openai)))

(defun gptel-model-updater--build-url (backend provider-type &optional api-key)
  "Build the models list URL for BACKEND given PROVIDER-TYPE.
API-KEY is used for providers that require it in the query string."
  (let ((host (gptel-backend-host backend))
        (endpoint (gptel-backend-endpoint backend))
        (protocol (or (gptel-backend-protocol backend) "https")))
    (pcase provider-type
      ('gemini
       (format "%s://%s/v1beta/models?key=%s&pageSize=1000" protocol host (or api-key "")))
      ('ollama
       (format "%s://%s/api/tags" protocol host))
      (_
       (let* ((base-path (replace-regexp-in-string "chat/completions.*" "" (or endpoint "")))
              (models-url (concat base-path "models")))
         (format "%s://%s%s" protocol host models-url))))))

(defun gptel-model-updater--build-headers (provider-type api-key)
  "Build request headers for PROVIDER-TYPE with API-KEY."
  (append '(("Content-Type" . "application/json"))
          (pcase provider-type
            ('openai
             (when api-key
               `(("Authorization" . ,(concat "Bearer " api-key)))))
            ;; Gemini uses query param for key, no auth header needed
            (_ nil))))

(defun gptel-model-updater--get-api-key (backend key-source)
  "Get API key for BACKEND from KEY-SOURCE.
Bind `gptel-backend' while resolving KEY-SOURCE so auth-source
lookups use BACKEND's host instead of the current chat backend."
  (when key-source
    (let ((gptel-backend backend))
      (ignore-errors (gptel--get-api-key key-source)))))

(defun gptel-model-updater--fetch-models (backend-name provider-type url headers callback)
  "Fetch models from URL and call CALLBACK with the result.
BACKEND-NAME is used for messages.
PROVIDER-TYPE is `openai', `ollama', or `gemini'.
URL is the endpoint to fetch from.
HEADERS is the request headers alist.
CALLBACK is called with (success raw-data error-message)."
  (message "GPTel-Model-Updater: Contacting %s..." backend-name)
  (let* ((args (list "--silent" "--max-time" (number-to-string gptel-model-updater-timeout)))
         (output-buf (generate-new-buffer " *gptel-model-updater-curl*")))
    (dolist (h headers)
      (setq args (append args (list "-H" (format "%s: %s" (car h) (cdr h))))))
    (setq args (append args (list url)))
    (make-process
     :name (format "gptel-model-updater-%s" backend-name)
     :buffer output-buf
     :command (cons "curl" args)
     :noquery t
     :sentinel
     (lambda (proc event)
       (when (string-match-p "finished" event)
         (with-current-buffer (process-buffer proc)
           (let* ((body (buffer-string))
                  (json-object-type 'alist)
                  (json-key-type 'symbol)
                  (response (condition-case nil
                                (json-read-from-string body)
                              (error nil))))
             (kill-buffer (process-buffer proc))
             (if (not response)
                 (funcall callback nil nil "Failed to parse JSON")
               (let ((raw-data (pcase provider-type
                                 ('gemini (alist-get 'models response))
                                 ('ollama (alist-get 'models response))
                                 (_ (alist-get 'data response)))))
                 (when (vectorp raw-data)
                   (setq raw-data (append raw-data nil)))
                 (funcall callback t raw-data nil)))))
         (when (buffer-live-p (process-buffer proc))
           (kill-buffer (process-buffer proc))))
       (when (string-match-p "\\(exited\\|failed\\)" event)
         (when (buffer-live-p (process-buffer proc))
           (kill-buffer (process-buffer proc)))
         (funcall callback nil nil (format "curl process %s" (string-trim event))))))))

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
Iterates over `gptel-model-updater-backends' and returns their name strings."
  (cl-loop for sym in (gptel-model-updater--backends)
           when (and (symbolp sym) (boundp sym))
           collect (gptel-backend-name (symbol-value sym))))

(defun gptel-model-updater--random-model (models)
  "Return a random model from MODELS."
  (nth (random (length models)) models))

(defun gptel-model-updater--pick-backend-model ()
  "Pick the first available managed backend and a random model."
  (catch 'found
    (dolist (backend-symbol (gptel-model-updater--backends))
      (when-let* ((backend (and (symbolp backend-symbol)
                                (boundp backend-symbol)
                                (symbol-value backend-symbol)))
                  (models (gptel-backend-models backend)))
        (throw 'found (cons backend (gptel-model-updater--random-model models)))))))

(defun gptel-model-updater--available-backends ()
  "Return configured backends that have model lists."
  (cl-loop for backend-symbol in (gptel-model-updater--backends)
           when (and (symbolp backend-symbol)
                     (boundp backend-symbol)
                     (gptel-backend-models (symbol-value backend-symbol)))
           collect (symbol-value backend-symbol)))

(defun gptel-model-updater--read-backend-model (&optional prompt-prefix)
  "Read a backend and model interactively.
PROMPT-PREFIX is prepended to completion prompts."
  (let* ((backends (gptel-model-updater--available-backends))
         (prompt-prefix (or prompt-prefix ""))
         (backend-name (completing-read
                        (format "%sBackend: " prompt-prefix)
                        (mapcar #'gptel-backend-name backends)
                        nil t))
         (backend (cl-find backend-name backends
                           :key #'gptel-backend-name
                           :test #'string=))
         (model-name (completing-read
                      (format "%sModel: " prompt-prefix)
                      (mapcar #'symbol-name (gptel-backend-models backend))
                      nil t)))
    (cons backend (intern model-name))))

(defun gptel-model-updater--set-choice (backend-variable model-variable choice)
  "Set BACKEND-VARIABLE and MODEL-VARIABLE from CHOICE."
  (when choice
    (set backend-variable (car choice))
    (set model-variable (cdr choice))))

(defun gptel-model-updater--target-label (target)
  "Return display label for external TARGET."
  (or (nth 2 target) (symbol-name (car target))))

(defun gptel-model-updater--select-external-targets (interactivep)
  "Set configured external targets.
When INTERACTIVEP is non-nil, read each target with completion;
otherwise select each target randomly."
  (dolist (target gptel-model-updater-external-targets)
    (pcase-let ((`(,backend-variable ,model-variable . ,_) target))
      (when (and (symbolp backend-variable) (symbolp model-variable))
        (gptel-model-updater--set-choice
         backend-variable model-variable
         (if interactivep
             (gptel-model-updater--read-backend-model
              (format "%s " (gptel-model-updater--target-label target)))
           (gptel-model-updater--pick-backend-model)))))))

;;;###autoload
(defun gptel-model-updater-select-backend-models (&optional external quiet choice interactive-external)
  "Select GPTel backend/model from refreshed model lists.
Interactively, read backend and model with completion.  Otherwise,
backends are tried in `gptel-model-updater-backends' order.  The first backend
with models is selected, and one model is chosen randomly.

When EXTERNAL is non-nil, also set variable pairs from
`gptel-model-updater-external-targets'.  Interactively, EXTERNAL is
enabled by a C-u prefix and each external target is selected
separately.  When QUIET is non-nil, do not print the final
selection.  CHOICE is a cons of BACKEND and MODEL."
  (interactive
   (let ((external current-prefix-arg))
     (list external
           nil
           (gptel-model-updater--read-backend-model "GPTel ")
           external)))
  (setq choice (or choice (gptel-model-updater--pick-backend-model)))
  (gptel-model-updater--set-choice 'gptel-backend 'gptel-model choice)
  (when external
    (gptel-model-updater--select-external-targets interactive-external))
  (unless quiet
    (message "GPTel: backend=%s model=%s%s"
             (and (boundp 'gptel-backend) gptel-backend
                  (gptel-backend-name gptel-backend))
             (and (boundp 'gptel-model) gptel-model)
             (if external
                 (mapconcat
                  (lambda (target)
                    (pcase-let ((`(,backend-variable ,model-variable . ,_) target))
                      (format "\n%s: backend=%s model=%s"
                              (gptel-model-updater--target-label target)
                              (and (boundp backend-variable)
                                   (symbol-value backend-variable)
                                   (gptel-backend-name (symbol-value backend-variable)))
                              (and (boundp model-variable)
                                   (symbol-value model-variable)))))
                  gptel-model-updater-external-targets
                  "")
               ""))))

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
         (key-source (gptel-backend-key backend))
         (api-key (gptel-model-updater--get-api-key backend key-source)))
    (if (and (memq provider '(openai gemini)) key-source (not api-key))
        (message "GPTel-Model-Updater: Skipping %s, no API key found" backend-name)
      (let ((fetch-url (or url (gptel-model-updater--build-url backend provider api-key)))
            (headers (gptel-model-updater--build-headers provider api-key)))
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
                          backend-name (length new-models))
                 (run-hook-with-args 'gptel-model-updater-after-update-hook
                                     backend-name backend new-models))))))))))

;;;###autoload
(defun gptel-model-updater-update-all ()
  "Update models for all configured GPTel backends."
  (interactive)
  (dolist (sym (gptel-model-updater--backends))
    (when (and (symbolp sym) (boundp sym))
      (let ((name (gptel-backend-name (symbol-value sym))))
        (condition-case err
            (gptel-model-updater-update-backend name)
          (error (message "GPTel-Model-Updater: Failed to update %s: %s" name err)))))))

(provide 'gptel-model-updater)
;;; gptel-model-updater.el ends here
