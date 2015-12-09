(require-extension test)
(require-extension tila-core)

(test-group
 "config API well-formed"
 (let* ((my-tila (tila '((output . i3) (color . yellow)) (element say-hello #:color "#123456")))
        (target (tila-target my-tila))
        (color (tila-default-color my-tila))
        (elements (tila-elements my-tila)))
   (test-assert "target correct" (eqv? 'i3 target))
   (test-assert "color correct" (eqv? 'yellow color))
   (test-group
    "element tests"
    (test "get exactly one element" 1 (length elements))
    (let ((elm (first elements)))
      (test "element produces expected output" (say-hello) ((element-proc elm)))
      (test "element has the correct color" "#123456" (element-color elm))))))

(test-group
 "different element procedures"
 (let* ((expected "say what?!")
        (argument " hello!")
        (my-string-element (element expected))
        (my-basic-element (element (lambda () expected)))
        (my-argy-element (element (lambda (str) (string-append expected argument))
                           #:args '(argument))))
   (test "string" expected ((element-proc my-string-element)))
   (test "closure" expected ((element-proc my-basic-element)))
   (test "with arguments" (string-append expected argument) ((element-proc my-argy-element)))))

(test-exit)
