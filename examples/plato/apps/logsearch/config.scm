;; configuration
(;; indicate log files with glob format.
 (log-glob . "/your/log/dir/**/*.log")
 ;; regular expression of starting log line.
 ;; yyyy-mm-dd hh:mi:ss,zzz level
 (log-head . "\\d{4}-\\d{2}-\\d{2}\\s+\\d{2}:\\d{2}:\\d{2},\\d{3}\\s+[A-Z]{4,5}")
 )
