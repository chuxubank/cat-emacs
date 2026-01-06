;; -*- lexical-binding: t; -*-

(use-package logview
  :mode ("\\<log\\>.*\\.\\(txt\\|gz\\)" . logview-mode)
  :hook (logview-mode . hl-line-mode)
  :custom
  (datetime-timezone 'Asia/Shanghai)
  (logview-additional-timestamp-formats
   '(("LogCat"
      (java-pattern . "MM-dd HH:mm:ss.SSS"))
     ("LogUtil"
      (java-pattern . "HH:mm:ss.SSS"))
     ("Zscaler"
      (java-pattern . "yyyy-MM-dd HH:mm:ss.SSSSSS(Z)"))
     ("sing-box"
      (java-pattern . "Z yyyy-MM-dd HH:mm:ss"))))
  (logview-additional-level-mappings
   '(("LogCat"
      (error "E" "F" "S")
      (warning "W")
      (information "I")
      (debug "D")
      (trace "V"))
     ("Xray"
      (error "Error")
      (warning "Warning")
      (information "Info")
      (debug "Debug"))
     ("Zscaler"
      (error "ERR")
      (warning "WRN")
      (information "INF")
      (debug "DBG"))
     ("Rclone"
      (error "ERROR" "CRITICAL")
      (warning "NOTICE")
      (information "INFO")
      (debug "DEBUG"))))
  (logview-additional-submodes
   '(("Luna"
      (format . "TIMESTAMP IGNORED LEVEL T: <<RX:THREAD:.+?>> NAME - MESSAGE")
      (levels . "Logback"))
     ("LogCat"
      (format . "TIMESTAMP IGNORED THREAD LEVEL NAME: MESSAGE")
      (levels . "LogCat")
      (timestamp "LogCat"))
     ("LogUtil"
      (format . "TIMESTAMP LEVEL/NAME [THREAD, IGNORED]: MESSAGE")
      (levels . "LogCat")
      (timestamp "LogUtil"))
     ("Xray"
      (format . "TIMESTAMP [LEVEL]<<RX:THREAD: \\[[^]]+\\] \\| >>NAME: MESSAGE")
      (levels . "Xray"))
     ("sing-box"
      (format . "TIMESTAMP LEVEL<<RX:IGNORED: \\[\\|>><<RX:THREAD:[0-9]+\\|>><<RX:IGNORED: [^]]+\\] \\| >>NAME: MESSAGE")
      (levels . "SLF4J")
      (timestamp "sing-box"))
     ("Zscaler"
      (format . "TIMESTAMP[IGNORED:THREAD] LEVEL MESSAGE")
      (levels . "Zscaler")
      (timestamp "Zscaler"))
     ("Rclone"
      (format . "TIMESTAMP LEVEL<<RX:IGNORED: *:>><<RX:THREAD:.+?: \\| >>MESSAGE")
      (levels . "Rclone")))))
