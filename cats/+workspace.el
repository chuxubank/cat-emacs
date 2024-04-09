;; -*- lexical-binding: t; -*-

(use-package tab-bar
  :hook
  (tab-bar-mode . tab-bar-history-mode)
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-show 1)
  (tab-bar-tab-hints t))

(use-package burly
  :hook
  (after-init . burly-tabs-mode))

(use-package bufler
  :hook
  (burly-tabs-after . bufler-workspace-mode))

(use-package tabspaces
  :disabled
  :hook
  (after-init . tabspaces-mode)
  :custom
  (tabspaces-initialize-project-with-todo nil)
  (tabspaces-session-file (concat cat-etc-dir "tabsession.el"))
  :config
  (with-eval-after-load 'consult
    ;; hide full buffer list (still available with "b" prefix)
    (consult-customize consult--source-buffer :hidden t :default nil)
    ;; set consult-workspace buffer list
    (defvar consult--source-workspace
      (list :name     "Workspace Buffers"
            :narrow   ?w
            :history  'buffer-name-history
            :category 'buffer
            :state    #'consult--buffer-state
            :default  t
            :items    (lambda () (consult--buffer-query
                                  :predicate #'tabspaces--local-buffer-p
                                  :sort 'visibility
                                  :as #'buffer-name)))

      "Set workspace buffer list for consult-buffer.")
    (add-to-list 'consult-buffer-sources 'consult--source-workspace)))
