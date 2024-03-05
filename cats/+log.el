;; -*- lexical-binding: t; -*-

(use-package logview
  :mode ("\\<log\\>.*\\.\\(txt\\|gz\\)" . logview-mode)
  :hook (logview-mode . hl-line-mode)
  :custom
  (datetime-timezone 'Asia/Shanghai)
  (logview-cache-filename (concat cat-cache-dir "logview-cache.extmap"))
  (logview-views-file (concat cat-etc-dir "logview.views"))
  (logview-additional-timestamp-formats
   '(("LogCat"
      (java-pattern . "MM-dd HH:mm:ss.SSS"))
     ("LogUtil"
      (java-pattern . "HH:mm:ss.SSS"))
     ("Zscaler"
      (java-pattern . "yyyy-MM-dd HH:mm:ss.SSSSSS(Z)"))))
  (logview-additional-level-mappings
   '(("LogCat"
      (error "E" "F" "S")
      (warning "W")
      (information "I")
      (debug "D")
      (trace "V"))
     ("Zscaler"
      (error "ERR")
      (warning "WRN")
      (information "INF")
      (debug "DBG"))))
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
     ("Zscaler"
      (format . "TIMESTAMP[IGNORED:THREAD] LEVEL MESSAGE")
      (levels . "Zscaler")
      (timestamp "Zscaler")))))
