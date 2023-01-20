;; -*- lexical-binding: t; -*-

(defvar cat-dark-mode-hook nil)
(defvar cat-light-mode-hook nil)
(defvar cat-theme-refresh-hook nil)

(cond
 ((featurep 'nano)
  (add-hook 'cat-dark-mode-hook #'nano-theme-set-dark)
  (add-hook 'cat-light-mode-hook #'nano-theme-set-light)
  (add-hook 'cat-theme-refresh-hook #'nano-refresh-theme))
 ((featurep 'nano-theme)
  (add-hook 'cat-dark-mode-hook #'nano-dark)
  (add-hook 'cat-light-mode-hook #'nano-light)
  (with-eval-after-load 'org
    (add-hook 'cat-theme-refresh-hook #'+org-buffers-refresh))))

(defun cat-dark-mode-p ()
  (cond
   (IS-WSL     (string-match-p "-Darker" (getenv "GTK_THEME")))
   (IS-WINDOWS (string= "0" (string-trim (shell-command-to-string "powershell.exe -WindowStyle Hidden -C Get-ItemPropertyValue -Path HKCU://Software/Microsoft/Windows/CurrentVersion/Themes/Personalize -Name AppsUseLightTheme"))))
   (IS-LINUX   (string-match-p "-dark" (shell-command-to-string "gsettings get org.gnome.desktop.interface gtk-theme")))
   (IS-MACPORT (string= (plist-get (mac-application-state) :appearance) "NSAppearanceNameDarkAqua"))
   (IS-MACPLUS (string= "dark" ns-system-appearance))
   (IS-MAC     (string= "Dark" (string-trim (shell-command-to-string "defaults read -g AppleInterfaceStyle"))))))

(defun cat-load-theme (&optional color)
  (interactive)
  (mapc 'disable-theme custom-enabled-themes)
  (when (or (display-graphic-p)
	    (daemonp))
    (if (cat-dark-mode-p)
	    (run-hooks 'cat-dark-mode-hook)
	  (run-hooks 'cat-light-mode-hook))
    (run-hooks 'cat-theme-refresh-hook)))

(add-hook 'after-init-hook #'cat-load-theme)

(when IS-MACPORT
  (add-hook 'mac-effective-appearance-change-hook #'cat-load-theme))

(when IS-MACPLUS
  (add-hook 'ns-system-appearance-change-functions #'cat-load-theme))
