;; -*- lexical-binding: t; -*-

(use-package groovy-mode
  :mode "\\.gradle\\'")

(use-package gradle
  :demand t
  :vc (gradle-el :url "https://git.sr.ht/~vhallac/gradle-el")
  :init
  (defun gradle--with-project-el-root (func)
    "Execute `func' inside eclim's project root"
    (if (not (require 'project nil t))
        (gradle--with-current-directory func)
      (let ((default-directory (project-root (project-current nil default-directory))))
        (funcall func))))
  :commands
  (gradle-discover-tasks gradle-run)
  :custom
  (gradle-with-project-root-func #'gradle--with-project-el-root)
  :config
  (defun gradle--executable-path ()
    gradle-executable)
  (defun gradle--cache-task-list (root)
    "Run gradle, get a list of tasks, parse the output, and cache the result.
This function stores the list of tasks associated with the
specified directory, ROOT."
    (message "Running gradle. Please wait...")
    ;; I need to run 'gradle tasks' twice in the code below. For
    ;; multi-project setups the output of 'gradle tasks --all' does not
    ;; contain ouput of 'gradle tasks', so I need to execute both to get
    ;; the full list.
    ;;
    ;; I may need to revisit this code to eliminate one of the
    ;; executions for single project setups, but the extra wait should
    ;; be OK for now.
    (let* ((default-directory root)
           (gradle-cmdline (concat (gradle--executable-path) " -q tasks"))
           (output (shell-command-to-string gradle-cmdline))
           (tasks (gradle--parse-tasks output))
           (output-all (shell-command-to-string (concat gradle-cmdline " --all")))
           (tasks-all (gradle--parse-tasks output-all))
           (combined-tasks (delete-dups (append tasks tasks-all)))
           (old-cache (assoc root gradle-tasks-for-path)))
      (when old-cache
        (delq old-cache gradle-tasks-for-path))
      (add-to-list 'gradle-tasks-for-path (cons root combined-tasks))
      (message "Done")
      combined-tasks)))

(use-package flymake-gradle
  :hook (flymake-mode . flymake-gradle-setup))

(use-package flycheck-gradle
  :hook (flycheck-mode . flycheck-gradle-setup))
