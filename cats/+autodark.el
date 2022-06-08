;; -*- lexical-binding: t; -*-

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
  (when (display-graphic-p)
    (if (cat-dark-mode-p)
	(nano-dark)
      (nano-light))))

(add-hook 'after-init-hook #'cat-load-theme)

(when IS-MACPORT
  (add-hook 'mac-effective-appearance-change-hook #'cat-load-theme))

(when IS-MACPLUS
  (add-hook 'ns-system-appearance-change-functions #'cat-load-theme))
