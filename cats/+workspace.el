;; -*- lexical-binding: t; -*-

(pretty-hydra-define cat-workspace
  (:color teal :title (+with-icon "nf-oct-codespaces" "Workspace"))
  ("" ()))

(use-package project
  :ensure nil
  :pretty-hydra
  (cat-workspace
   ("Project"
    (("p" #'project-remember-projects-under "remember all")
     ("f" #'project-forget-project "forget")
     ("F" #'project-forget-zombie-projects "forget zombie")
     ("D" #'project-forget-projects-under "forget all")))))

(use-package tab-bar
  :ensure nil
  :hook
  (tab-bar-mode . tab-bar-history-mode)
  :custom
  (tab-bar-close-button-show t)
  (tab-bar-new-button-show t)
  (tab-bar-show 1)
  (tab-bar-tab-hints t)
  (tab-bar-select-tab-modifiers '(super))
  :pretty-hydra
  (cat-workspace
   ("Tab-bar"
    (("t" #'tab-bar-mode "mode")
     ("h" #'tab-bar-history-mode "history")
     ("n" #'tab-bar-new-tab "new tab")
     ("N" #'tab-bar-new-tab-to "new tab to")
     ("k" #'tab-bar-close-tab "close tab")
     ("r" #'tab-bar-rename-tab "rename")
     ("R" #'tab-bar-rename-tab-by-name "rename by name")))))

(use-package burly
  :disabled
  :hook
  (after-init . burly-tabs-mode)
  :pretty-hydra
  ((:color teal :title (+with-icon "nf-oct-bookmark" "Burly"))
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
     ("w" #'burly-kill-windows-url "windows"))))
  (cat-workspace
   ("Plugin"
    (("m" #'burly-hydra/body "burly")))))

(use-package bufler
  :disabled
  :hook
  (burly-tabs-after . bufler-workspace-mode)
  :bind
  ([remap list-buffers] . bufler)
  :custom
  (bufler-workspace-mode-lighter (+with-icon "nf-oct-codespaces"))
  :pretty-hydra
  ((:color teal :title (+with-icon "nf-oct-project" "Bufler"))
   ("Workspace"
    (("b" #'bufler-workspace-switch-buffer "switch buffer")
     ("f" #'bufler-workspace-focus-buffer "focus buffer")
     ("o" #'bufler-workspace-open "open")
     ("r" #'bufler-workspace-reset "reset")
     ("s" #'bufler-workspace-save "save"))
    "Other"
    (("S" #'bufler-workspace-set "set")
     ("N" #'bufler-workspace-buffer-name-workspace "set workspace")
     ("F" #'bufler-workspace-frame-set "set frame"))))
  (cat-workspace
   ("Plugin"
    (("b" #'bufler-hydra/body "bufler")))))

(use-package tabspaces
  :disabled
  :hook
  (after-init . tabspaces-mode)
  :custom
  (tabspaces-include-buffers nil)
  (tabspaces-initialize-project-with-todo nil)
  :pretty-hydra
  ((:color teal :title (+with-icon "nf-md-tab" "Tabspaces"))
   ("Buffer"
    (("C" tabspaces-clear-buffers "clear")
     ("b" tabspaces-switch-to-buffer "switch buffer")
     ("r" tabspaces-remove-current-buffer "remove buffer")
     ("R" tabspaces-remove-selected-buffer "remove select buffer"))
    "Workspace"
    (("o" tabspaces-open-or-create-project-and-workspace "open project")
     ("k" tabspaces-kill-buffers-close-workspace "kill buffer close")
     ("t" tabspaces-switch-buffer-and-tab "switch to buffer tab")
     ("s" tabspaces-switch-or-create-workspace "switch"))
    "Session"
    (("p" #'tabspaces-save-current-project-session "save current project")
     ("a" #'tabspaces-restore-session "restore"))))
  (cat-workspace
   ("Plugin"
    (("s" #'tabspaces-hydra/body "tabspaces"))))
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

(use-package activities
  :hook
  (after-init . activities-mode)
  (after-init . activities-tabs-mode)
  :custom
  (activities-kill-buffers t)
  :pretty-hydra
  ((:color teal :title (+with-icon "nf-cod-layout_activitybar_left" "Activities"))
   ("Manage"
    (("n" activities-new)
     ("d" activities-define)
     ("r" activities-rename)
     ("D" activities-discard)
     ("a" activities-resume)
     ("s" activities-suspend)
     ("k" activities-kill)
     ("g" activities-revert))
    "View"
    (("b" activities-switch-buffer)
     ("l" activities-list)
     ("RET" activities-switch))))
  (cat-workspace
   ("Plugin"
    (("a" #'activities-hydra/body "activities")))))

(use-package sow
  :ensure nil
  :delight
  :hook (after-init . sow-mode))
