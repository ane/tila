;;; -*- scheme -*-
;;; vim:ft=scheme

(use tila-core)

(tila 'i3
      (element say-hello #:color "#008080")
      (element hostname #:color "red")
      (element date-and-time))
