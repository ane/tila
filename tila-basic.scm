(require-extension srfi-19)

(define (say-hello)
  "Hello there!")

(define (hostname)
  (get-host-name))

(define (date-and-time #!optional (format "~Y-~m-~d ~H:~M:~S"))
  (format-date format (current-date)))
