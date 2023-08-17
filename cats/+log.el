;; -*- lexical-binding: t; -*-

(use-package logview
  :mode ("/log\\b" . logview-mode)
  :custom
  (datetime-timezone 'Asia/Shanghai)
  (logview-cache-filename (concat cat-cache-dir "logview-cache.extmap"))
  (logview-views-file (concat cat-etc-dir "logview.views"))
  (logview-additional-timestamp-formats
   '(("LogCat"
      (java-pattern . "MM-dd HH:mm:ss.SSS"))))
  (logview-additional-level-mappings
   '(("LogCat"
      (error "E" "F" "S")
      (warning "W")
      (information "I")
      (debug "D")
      (trace "V"))))
  (logview-additional-submodes
   '(("Luna"
      (format . "TIMESTAMP IGNORED LEVEL T: <<RX:THREAD:.+?>> NAME - MESSAGE")
      (levels . "Logback"))
     ("LogCat"
      (format . "TIMESTAMP IGNORED THREAD LEVEL NAME: MESSAGE")
      (levels . "LogCat")
      (timestamp "LogCat")))))
