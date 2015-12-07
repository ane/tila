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
                   date-and-time)

(import chicken scheme posix)

(use srfi-19
     srfi-1
     srfi-13
     srfi-34
     medea
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

(define (element proc #!key (color "#ffffff"))
  (make-element proc color))

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
         `((name . "derp")
           (full_text . ,(proc))
           )))
     elements)))

(define (output-to-i3 tila)
  (let ((processed (process-elements tila)))
    (json->string (list->vector processed))))

(define (print-tila tila)
  (cond
   ((eq? (tila-target tila) 'json)
    (output-to-i3 tila)) 
   (else (display "asdf"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (say-hello)
  "Hi there!")

(define (hostname)
  (get-host-name))

(define (date-and-time #!optional (format "~Y-~m-~d ~H:~M:~S"))
  (format-date format (current-date))))
