(use srfi-1
     srfi-13
     srfi-34
     medea
     extras)

(import tila-core)

(define *default-config*
  `(,(string-append (get-environment-variable "HOME") "/.tila")
    ,(string-append (get-environment-variable "HOME") "/.config/tila/config.scm")
    "/etc/tila/tila.scm"))

(define *homepage* "https://ane.github.io/tila/")

(define (get-config)
  (let ((cfgs (filter file-exists? *default-config*)))
    (unless (null-list? cfgs)
        (car cfgs))))

(define (load-config)
  (set! *tila-state* #f)
  (let ((cfg (get-config)))
    (if (and (string? cfg)
             (not (string-null? cfg)))
        (load cfg)
        (raise 'no-config))))

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

(define (run-tila)
  (guard (e [(eq? e 'no-config) (report-and-exit say-config-missing)]
            [(eq? e 'no-state)  (report-and-exit say-state-missing)])
    (load-config)
    (let ((state (get-tila-state)))
      (if state
          (map (lambda (element)
                 (format #t "An element is saying: ~a and it's color is: ~a\n"
                         ((element-proc element))
                         (element-color element)))
               (tila-elements state))
          (raise 'no-state)))))

(define (begin-tila-i3 initial-state)
  (define interval 1)
  (define (prn-status-and-sleep state)
    (format (current-output-port)    ; stdout
            "~a~%"               
            (string-append "," (print-tila state 'json)))
    (flush-output (current-output-port))
    (sleep interval))
  (begin
    (format #t "{ \"version\": 1 }~%")
    (format #t "[~%")                ; infinite array
    (format #t "[]~%")
    (prn-status-and-sleep initial-state)
    (let tila-loop ((foo #t))
      (prn-status-and-sleep initial-state)
      (tila-loop #t))))

(define (begin-tila)
  (guard (e [(eq? e 'no-config) (report-and-exit say-config-missing)]
            [(eq? e 'no-state)  (report-and-exit say-state-missing)])
    (load-config)
    (define state (get-tila-state))
    (if state
        (cond
         ((eq? (tila-target state) 'i3)
          (begin-tila-i3 state)))
        (raise 'no-state))))

(begin-tila)
