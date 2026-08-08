;; -*- lexical-binding: t; -*-

(mode-transient-define-prefix cat-workspace ()
  :description (+with-icon "nf-oct-codespaces" nil " Workspace"))

(use-package project
  :ensure nil
  :custom
  (project-vc-merge-submodules nil)
  :transient
  (cat-workspace
   ["Project"
    ("p" "remember under" project-remember-projects-under)
    ("f" "forget" project-forget-project)
    ("F" "forget zombie" project-forget-zombie-projects)
    ("D" "forget under" project-forget-projects-under)]))

(use-package tab-bar
  :ensure nil
  :hook
  (after-init . tab-bar-mode)
  (tab-bar-mode . tab-bar-history-mode)
  :custom
  (tab-bar-close-button-show t)
  (tab-bar-new-button-show t)
  (tab-bar-show 1)
  (tab-bar-tab-hints t)
  (tab-bar-select-tab-modifiers '(super))
  :transient
  (cat-workspace
   ["Tab-bar"
    ("t" "mode" tab-bar-mode)
    ("h" "history" tab-bar-history-mode)
    ("n" "new tab" tab-bar-new-tab)
    ("N" "new tab to" tab-bar-new-tab-to)
    ("k" "close tab" tab-bar-close-tab)
    ("r" "rename" tab-bar-rename-tab)
    ("R" "rename by name" tab-bar-rename-tab-by-name)]))

(use-package burly
  :cat
  :hook
  (after-init . burly-tabs-mode)
  :transient
  (cat-burly
   (:description (+with-icon "nf-oct-bookmark" nil " Burly"))
   ["Bookmark"
    ("o" "open" burly-open-bookmark)
    ("l" "open last" burly-open-last-bookmark)
    ("r" "reset tab" burly-reset-tab)
    ("f" "frame" burly-bookmark-frames)
    ("w" "windows" burly-bookmark-windows)]
   ["URL"
    ("u" "open" burly-open-url)
    ("b" "buffer" burly-kill-buffer-url)
    ("F" "frame" burly-kill-frames-url)
    ("W" "windows" burly-kill-windows-url)])
  (cat-workspace
   ["Plugin"
    ("m" "burly" cat-burly)]))

(use-package bufler
  :cat
  :hook
  (burly-tabs-after . bufler-workspace-mode)
  :bind
  ([remap list-buffers] . bufler)
  :custom
  (bufler-workspace-mode-lighter (+with-icon "nf-oct-codespaces"))
  :transient
  (cat-bufler
   (:description (+with-icon "nf-oct-project" nil " Bufler"))
   ["Workspace"
    ("b" "switch buffer" bufler-workspace-switch-buffer)
    ("f" "focus buffer" bufler-workspace-focus-buffer)
    ("o" "open" bufler-workspace-open)
    ("r" "reset" bufler-workspace-reset)
    ("s" "save" bufler-workspace-save)]
   ["Other"
    ("S" "set" bufler-workspace-set)
    ("N" "set workspace" bufler-workspace-buffer-name-workspace)
    ("F" "set frame" bufler-workspace-frame-set)])
  (cat-workspace
   ["Plugin"
    ("b" "bufler" cat-bufler)]))

(use-package tabspaces
  :cat
  :hook
  (after-init . tabspaces-mode)
  :custom
  (tabspaces-include-buffers nil)
  (tabspaces-initialize-project-with-todo nil)
  :transient
  (cat-tabspaces
   (:description (+with-icon "nf-md-tab" nil " Tabspaces"))
   ["Buffer"
    ("C" "clear" tabspaces-clear-buffers)
    ("b" "switch buffer" tabspaces-switch-to-buffer)
    ("r" "remove buffer" tabspaces-remove-current-buffer)
    ("R" "remove select buffer" tabspaces-remove-selected-buffer)]
   ["Workspace"
    ("o" "open project" tabspaces-open-or-create-project-and-workspace)
    ("k" "kill buffer close" tabspaces-kill-buffers-close-workspace)
    ("t" "switch to buffer tab" tabspaces-switch-buffer-and-tab)
    ("s" "switch" tabspaces-switch-or-create-workspace)]
   ["Session"
    ("p" "save current project" tabspaces-save-current-project-session)
    ("a" "restore" tabspaces-restore-session)])
  (cat-workspace
   ["Plugin"
    ("s" "tabspaces" cat-tabspaces)])
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
  :transient
  (cat-activities
   (:description (+with-icon "nf-cod-layout_activitybar_left"
                             nil " Activities"))
   ["Manage"
    ("n" activities-new)
    ("d" activities-define)
    ("r" activities-rename)
    ("D" activities-discard)
    ("a" activities-resume)
    ("s" activities-suspend)
    ("k" activities-kill)
    ("g" activities-revert)]
   ["View"
    ("b" activities-switch-buffer)
    ("l" activities-list)
    ("RET" activities-switch)])
  (cat-workspace
   ["Plugin"
    ("a" "activities" cat-activities)]))

(use-package sow
  :vc (:url "https://github.com/cat-emacs/scroll-other-window")
  :delight
  :hook (after-init . sow-mode))

(use-package treemacs
  :font-role ui
  :bind
  ("M-0" . treemacs-select-window)
  :custom
  (treemacs-is-never-other-window t)
  (treemacs-text-scale -1))

(use-package treemacs-magit
  :demand t
  :after treemacs magit)

(use-package treemacs-tab-bar
  :demand t
  :after treemacs)

(use-package treemacs-nerd-icons
  :demand t
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package treemacs-activities
  :vc (:url "https://github.com/cat-emacs/treemacs-activities")
  :demand t
  :after (treemacs activities)
  :config
  (treemacs-set-scope-type 'Activities)
  :transient
  (cat-workspace
   ["Treemacs"
    ("e" "edit workspaces" treemacs-edit-workspaces)]))
