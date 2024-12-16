;; -*- lexical-binding: t; -*-

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
     ("\\.zip\\'"
      (concat "7z x -o\\*")
      (concat "7z x -o\\* -p"))
     ("\\.pub\\'"
      (concat "ssh-keygen -lv -f"))
     ("\\.md\\'"
      (concat "pandoc ? -o $(basename `?` .md).org --lua-filter=remove-header-attr.lua")
      (concat "pandoc ? -o $(basename `?` .md).org"))))
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
