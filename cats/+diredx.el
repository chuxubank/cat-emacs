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

(add-hook 'dired-load-hook
          (lambda ()
	    ;; Bind dired-x-find-file.
            (setq dired-x-hands-off-my-keys nil
		  dired-guess-shell-gnutar "gtar")
            (load "dired-x")
            ))

(add-hook 'dired-mode-hook
          (lambda ()
	    (when IS-WINDOWS
	      (setq dired-omit-files
		    (concat dired-omit-files "\\|^ntuser\\(\\.dat\\|\\.ini\\).*")))
            (dired-omit-mode 1)
            ))
