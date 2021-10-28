(defun cat-dark-mode-p ()
  (cond
   (IS-WSL     (string-match-p "-Darker" (getenv "GTK_THEME")))
   (IS-WINDOWS (string= "0" (string-trim (shell-command-to-string "powershell.exe -WindowStyle Hidden -C Get-ItemPropertyValue -Path HKCU://Software/Microsoft/Windows/CurrentVersion/Themes/Personalize -Name AppsUseLightTheme"))))
   (IS-LINUX   (string-match-p "-dark" (shell-command-to-string "gsettings get org.gnome.desktop.interface gtk-theme")))
   (IS-MAC     (string= (plist-get (mac-application-state) :appearance) "NSAppearanceNameDarkAqua"))))

(defun cat-load-theme ()
  (interactive)
  (if (cat-dark-mode-p)
      (nano-dark)
    (nano-light)))

(add-hook 'after-init-hook #'cat-load-theme)

(define-key mac-apple-event-map [application-kvo effectiveAppearance] #'cat-load-theme)
