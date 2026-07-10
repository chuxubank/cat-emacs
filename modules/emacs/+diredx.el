;; -*- lexical-binding: t; -*-

;; Source - https://stackoverflow.com/a/25944631
;; Posted by killdash9
;; Retrieved 2026-05-06, License - CC BY-SA 3.0
(defun cat/ediff-marked-pair ()
  "Run ediff-files on a pair of files marked in dired buffer"
  (interactive)
  (let* ((marked-files (dired-get-marked-files nil nil))
         (other-win (get-window-with-predicate
                     (lambda (window)
                       (with-current-buffer (window-buffer window)
                         (and (not (eq window (selected-window)))
                              (eq major-mode 'dired-mode))))))
         (other-marked-files (and other-win
                                  (with-current-buffer (window-buffer other-win)
                                    (dired-get-marked-files nil)))))
    (cond ((= (length marked-files) 2)
           (ediff-files (nth 0 marked-files)
                        (nth 1 marked-files)))
          ((and (= (length marked-files) 1)
                (= (length other-marked-files) 1))
           (ediff-files (nth 0 marked-files)
                        (nth 0 other-marked-files)))
          ((= (length marked-files) 1)
           (let ((single-file (nth 0 marked-files)))
             (ediff-files single-file
                          (read-file-name
                           (format "Diff %s with: " single-file)
                           nil (if (string= single-file (dired-get-filename))
                                   nil
                                 (dired-get-filename)) t))))
          (t (error "mark no more than 2 files")))))

(use-package dired
  :ensure nil
  :ensure-system-package
  (7z . p7zip)
  :bind
  (:map ctl-x-map
        ("C-j" . dired-jump))
  (:map ctl-x-4-map
        ("C-j" . dired-jump-other-window))
  :custom
  (delete-by-moving-to-trash t)
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-guess-shell-alist-user
   '(("^app_logs.*\\.zip\\'"
      (format "7z x -o\\* -p%s" (password-store-get "Work/AA/Luna/Log")))
     ("\.?codecov.ya?ml\\'"
      "curl -X POST --data-binary @`?` https://codecov.io/validate")
     ("\\.csv\\'"
      "double-entry-generator translate"
      "double-entry-generator translate --provider ")
     ("\\.md\\'"
      (concat "pandoc ? -o $(basename `?` .md).org --lua-filter=remove-header-attr.lua")
      (concat "pandoc ? -o $(basename `?` .md).org"))
     ("\\.pub\\'"
      (concat "ssh-keygen -lv -f"))
     ("\\.plist\\'"
      (concat "plutil -p"))
     ("\\.srs\\'"
      (concat "sing-box rule-set decompile"))
     ("\\.tmpl\\'"
      "cat * | chezmoi execute-template")
     ("\\.yaml\\'"
      "fly -t $(yq -r '.targets | keys | .[0]' ~/.flyrc) validate-pipeline --enable-across-step --config"
      "fly -t $(yq -r '.targets | keys | .[0]' ~/.flyrc) set-pipeline --config ? --pipeline $(basename `?` .yaml)")
     ("\\.zip\\'"
      (concat "7z x -o\\*")
      (concat "7z x -o\\* -p"))
     (".*\\'"
      (format "7z a ?.zip")
      (format "7z a ?.zip -p"))))
  :config
  (let ((args (list "-ahlv" "--group-directories-first")))
    (when IS-BSD
      ;; Use GNU ls as `gls' from `coreutils' if available. Add `(setq
      ;; dired-use-ls-dired nil)' to your config to suppress the Dired warning
      ;; when not using GNU ls.
      (if-let (gls (executable-find "gls"))
          (setq insert-directory-program gls)
        ;; BSD ls doesn't support --group-directories-first
        (setq args (list (car args)))))
    (setq dired-listing-switches (string-join args " "))))

(use-package dired-x
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :custom
  (dired-x-hands-off-my-keys nil)
  :config
  (defun dired-omit-startup ()
    (or (assq 'dired-omit-mode minor-mode-alist)
        (setq minor-mode-alist
              (append '((dired-omit-mode
		                 (:eval (if (eq major-mode 'dired-mode)
				                    " " ""))))
		              minor-mode-alist))))
  (setq dired-omit-files
        (concat dired-omit-files
                (cond
                 (IS-WINDOWS
                  "\\|^ntuser\\(\\.dat\\|\\.ini\\).*")
                 (IS-MAC
                  "\\|.DS_Store")))))

(use-package nerd-icons-dired
  :delight " "
  :hook
  (dired-mode . nerd-icons-dired-mode))

(when IS-MAC
  (use-package dirvish
    :ensure-system-package
    (gls . coreutils)
    (fd . fd)
    (ffmpegthumbnailer . ffmpegthumbnailer)
    (mediainfo . mediainfo)))

(use-package dired-rsync
  :demand t
  :after dired
  :bind (:map dired-mode-map
              ("r r" . dired-rsync)))

(use-package dired-rsync-transient
  :demand t
  :after dired
  :bind (:map dired-mode-map
              ("r t" . dired-rsync-transient)))
