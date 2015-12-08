(module tila-core (tila
                   tila?
                   tila-target
                   tila-elements
                   element
                   element?
                   element-proc
                   element-color
                   get-tila-state
                   process-elements
                   print-tila
                   hostname
                   say-hello
                   date-and-time
                   )

(import chicken scheme posix)
(reexport tila-ffi)
(use tila-ffi)
(use srfi-19
     srfi-1
     srfi-13
     medea
     bind
     system
     extras)


(define *tila-state* #f)

(define-record-type <tila>
  (make-tila target elements)
  tila?
  (target tila-target)
  (elements tila-elements))

(define-record-type <element>
  (make-element proc color)
  element?
  (proc element-proc)
  (color element-color))

(define (element proc #!key (args '()) (color "#ffffff"))
  (let ((newproc
         (cond
          ((string? proc) (lambda () proc))
          ((procedure? proc) (lambda () (apply proc args))))))
    (make-element newproc color))) 

(define (tila output #!rest elems)
  (set! *tila-state* (make-tila output elems)))

(define (get-tila-state) *tila-state*)

(define (process-elements tila)
  (let ((elements (tila-elements tila)))
    (map-in-order
     (lambda (element)
       (let* ((proc (element-proc element))
              (kolor (element-color element))
              (info (procedure-information proc)))
         `((full_text . ,(proc))
           (color . ,(cond
                      ((symbol? kolor)
                       (symbol->string kolor))
                      ((string? kolor)
                       kolor)
                      (else "#ffffff"))))))
     elements)))

(define (output-to-i3 tila)
  (let ((processed (process-elements tila)))
    (json->string (list->vector processed))))

(define (print-tila tila fmt)
  (cond
   ((eq? fmt 'json)
    (output-to-i3 tila)) 
   (else (display "asdf"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (say-hello)
  "sup")

(define (hostname)
  (get-host-name))

(define (date-and-time #!optional (format "~Y-~m-~d ~H:~M:~S"))
  (format-date format (current-date)))

)
