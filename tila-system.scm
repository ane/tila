(require-extension srfi-1
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

(define (element proc #!key (color 'white))
  (make-element proc color))

(define (tila output #!rest elems)
  (set! *tila-state* (make-tila output elems)))

(define (get-tila-state) *tila-state*)

(define (process-elements tila)
  (let ((elements (tila-elements tila)))
    (map-in-order
     (lambda (element)
       (let* ((proc (element-proc element))
              (info (procedure-information proc)))
         `((content . ,(proc))
           (color . ,(element-color element)))))
     elements)))

;; (define (print-to-json tila)
;;    (write-json
;;     (map (lambda (element)
;;            (let ((content))))
;;          (process-elements tila))))

(define (tila-print tila)
  (cond
   (eq? (tila-target tila) 'json)
   (display "pladf") 
   (else (display "asdf"))))
