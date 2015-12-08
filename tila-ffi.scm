(module tila-ffi *

(import chicken scheme posix)
(use bind srfi-1 srfi-4 srfi-13)

(bind "int getloadavg(double loadavg[], int nelem);")
(define (load-average #!optional (nsamples 3))
  (let* ((samples (or (and (number? nsamples) (> nsamples 0) (< nsamples 4) nsamples) 1))
         (lavg (make-f64vector samples)))
    (getloadavg lavg 3)
    (let ((avgs (f64vector->list lavg)))
      (string-join (map-in-order number->string avgs) " "))))

)

