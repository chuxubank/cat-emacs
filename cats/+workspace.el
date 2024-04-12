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
  :disabled
  :hook
  (after-init . burly-tabs-mode)
  :pretty-hydra
  ((:color teal :title "Burly")
   ("Bookmark"
    (("o" #'burly-open-bookmark "open")
     ("l" #'burly-open-last-bookmark "open last")
     ("r" #'burly-reset-tab "reset tab")
     ("f" #'burly-bookmark-frames "frame")
     ("w" #'burly-bookmark-windows "windows"))
    "URL"
    (("u" #'burly-open-url "open")
     ("b" #'burly-kill-buffer-url "buffer")
     ("f" #'burly-kill-frames-url "frame")
     ("w" #'burly-kill-windows-url "windows")))))

(use-package bufler
  :disabled
  :hook
  (burly-tabs-after . bufler-workspace-mode)
  :bind
  ([remap list-buffers] . bufler)
  :custom
  (bufler-workspace-mode-lighter (nerd-icons-octicon "nf-oct-codespaces"))
  :pretty-hydra
  ((:color teal :title "Bufler")
   ("Workspace"
    (("b" #'bufler-workspace-switch-buffer "switch buffer")
     ("f" #'bufler-workspace-focus-buffer "focus buffer")
     ("o" #'bufler-workspace-open "open")
     ("r" #'bufler-workspace-reset "reset")
     ("s" #'bufler-workspace-save "save"))
    "Other"
    (("S" #'bufler-workspace-set "set")
     ("N" #'bufler-workspace-buffer-name-workspace "set workspace")
     ("F" #'bufler-workspace-frame-set "set frame")))))

(use-package activities
  :hook
  (after-init . activities-mode)
  (after-init . activities-tabs-mode)
  :pretty-hydra
  ((:color teal :title "Activities")
   ("Manage"
    (("n" activities-new)
     ("d" activities-define)
     ("a" activities-resume)
     ("s" activities-suspend)
     ("k" activities-kill)
     ("g" activities-revert))
    "View"
    (("b" activities-switch-buffer)
     ("l" activities-list)
     ("RET" activities-switch)))))

(use-package tabspaces
  :hook
  (after-init . tabspaces-mode)
  :custom
  (tabspaces-initialize-project-with-todo nil)
  (tabspaces-session-file (concat cat-etc-dir "tabsession.el"))
  :pretty-hydra
  ((:color teal :title "Tabspaces")
   ("Buffer"
    (("C" tabspaces-clear-buffers "clear")
     ("b" tabspaces-switch-to-buffer "switch buffer")
     ("d" tabspaces-close-workspace "close")
     ("k" tabspaces-kill-buffers-close-workspace "kill buffer close")
     ("o" tabspaces-open-or-create-project-and-workspace "open project")
     ("s" tabspaces-switch-or-create-workspace "switch")
     ("r" tabspaces-remove-current-buffer "remove buffer")
     ("R" tabspaces-remove-selected-buffer "remove select buffer"))))
  :config
  (tab-bar-rename-tab "Home")
  (when (get-buffer "*Messages*")
    (set-frame-parameter nil
                         'buffer-list
                         (cons (get-buffer "*Messages*")
                               (frame-parameter nil 'buffer-list))))
  (when (get-buffer "*dashboard*")
    (set-frame-parameter nil
                         'buffer-list
                         (cons (get-buffer "*dashboard*")
                               (frame-parameter nil 'buffer-list))))
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

(pretty-hydra-define cat-workspace
  (:color teal :title "Workspace")
  ("Plugin"
   (("a" #'activities-hydra/body "activities")
    ;; ("b" #'bufler-hydra/body "bufler")
    ;; ("m" #'burly-hydra/body "burly")
    ("t" #'tabspaces-hydra/body "tabspaces"))
   "Tab-bar"
   (("d" #'tab-bar-close-tab "close tab")
    ("r" #'tab-bar-rename-tab "rename")
    ("R" #'tab-bar-rename-tab-by-name "rename by name"))))
