(use srfi-1
     srfi-13
     srfi-18 ; repl thread
     medea
     extras)

(use tila-core)

(define *default-config*
  `(,(string-append (get-environment-variable "HOME") "/.tila")
    ,(string-append (get-environment-variable "HOME") "/.config/tila/config.scm")
    "/etc/tila/tila.scm"))

(define *homepage* "https://ane.github.io/tila/")

(define (get-config)
  (let ((cfgs (filter file-exists? *default-config*)))
    (and (not (null? cfgs)) (first cfgs))))

(define (load-config)
  (set! *tila-state* #f)
  (let ((cfg (get-config)))
    (if (and (string? cfg)
             (not (string-null? cfg)))
        (load cfg)
        (abort (make-property-condition 'no-config 'message "No config file found.")))))

(define (say-config-missing)
  (display "No configuration file found. Make sure one of these files exists: \n\n")
  (map (lambda (file)
         (format #t "\t* ~a\n" file))
       *default-config*)
  (display "\n"))

(define (say-state-missing)
  (display (string-append
            "The configurations did not load correctly. Make sure there's a call to \n"
            "(tila ...) in the configuration file. Please see the documentation at \n"
            *homepage*
            " for more information.\n")))

(define (report-and-exit proc #!optional (code 1))
  (proc)
  (exit 1))

(define (begin-tila-i3 initial-state)
  (define interval 1)
  (define stdout (open-output-file* fileno/stdout))
  (define (prn-status-and-sleep state)
    (format stdout "~a~%" (string-append "," (print-tila state 'json)))
    (flush-output stdout)
    (thread-sleep! interval))
  (begin
    (format stdout "{ \"version\": 1 }~%")
    (format stdout "[~%")                ; infinite array
    (format stdout "[]~%")
    (prn-status-and-sleep initial-state)
    (let tila-loop ()
      (prn-status-and-sleep initial-state)
      (tila-loop))))

(define (start-running-tila state)
  (cond
   ((eq? (tila-target state) 'i3)
    (begin-tila-i3 state))
   (else (begin-tila-i3 state))))

(define (begin-tila)
  (condition-case
      (begin
        (load-config)
        (define state (get-tila-state))
        (if state
            (start-running-tila state)
            (abort
             (make-property-condition 'no-state 'message "No tila configuration loaded."))))
    [e (no-config) (report-and-exit say-config-missing)]
    [e (no-state)  (report-and-exit say-state-missing)]
    [e (exn) (start-running-tila
              (tila '((output . i3))
                    (element
                        (format #f "ERROR: Failed to load your config: ~a: ~a."
                                (get-condition-property e 'exn 'message)
                                (get-condition-property e 'exn 'arguments))
                      #:color "red")))]
    [e (user-interrupt) (display "Interrupted.") (newline)]))

(unless (string-suffix? "csi" (program-name))
  (begin-tila))


