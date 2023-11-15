;; -*- lexical-binding: t; -*-

(setq delete-by-moving-to-trash t
      dired-dwim-target t
      dired-kill-when-opening-new-dired-buffer t
      dired-guess-shell-alist-user
      '(("\\.zip\\'"
         (concat "7z x" " -o" (file-name-sans-extension file))
         (concat "7z x" " -o" (file-name-sans-extension file) " -p"))))

(let ((args (list "-ahlv" "--group-directories-first")))
  (when IS-BSD
    ;; Use GNU ls as `gls' from `coreutils' if available. Add `(setq
    ;; dired-use-ls-dired nil)' to your config to suppress the Dired warning
    ;; when not using GNU ls.
    (if-let (gls (executable-find "gls"))
        (setq insert-directory-program gls)
      ;; BSD ls doesn't support --group-directories-first
      (setq args (list (car args)))))
  (setq dired-listing-switches (string-join args " ")))

(autoload 'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer." t)

(autoload 'dired-jump-other-window "dired-x"
  "Like \\[dired-jump] (dired-jump) but in other window." t)

(add-hook 'dired-load-hook
          (lambda ()
            ;; Bind dired-x-find-file.
            (setq dired-x-hands-off-my-keys nil)
            (load "dired-x")
            ))

(add-hook 'dired-mode-hook
          (lambda ()
            (when IS-WINDOWS
              (setq dired-omit-files
                    (concat dired-omit-files "\\|^ntuser\\(\\.dat\\|\\.ini\\).*")))
            (dired-omit-mode 1)
            ))

(define-key global-map "\C-x\C-j" 'dired-jump)
(define-key global-map "\C-x4\C-j" 'dired-jump-other-window)

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package dirvish
  :ensure-system-package
  (gls . coreutils)
  (fd . fd)
  (ffmpegthumbnailer . ffmpegthumbnailer)
  (mediainfo . mediainfo)
  :defer t
  :custom
  (dirvish-cache-dir (concat cat-cache-dir "dirvish/")))
