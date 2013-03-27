#lang racket

(require rackunit "compiler.rkt")

(define (check-prog program expected-output)
  (let [(program-output (compile-and-exec program "test-program"))
        (program-as-string (with-output-to-string (lambda () (write program))))]
    (check-equal?
      program-output
      expected-output
      (format "program text: ~a" program-as-string))))

(check-prog 5 "5")
(check-prog 5439 "5439")
(check-prog #\a "a")
(check-prog #\Z "Z")
