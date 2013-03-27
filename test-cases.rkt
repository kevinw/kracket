#lang racket

(require rackunit "compiler.rkt")

(define (check-prog program expected-output)
  (let [(program-output (compile-and-exec program "test-program"))
        (program-as-string (with-output-to-string (lambda () (write program))))]
    (check-equal?
      program-output
      expected-output
      (format "program text: ~a" program-as-string))))

(test-case
  "Primitives"

  (check-prog 5 "5")
  (check-prog 5439 "5439")
  (check-prog #\a "a")
  (check-prog #\Z "Z"))

(test-case
  "Unary expressions"

  (check-prog '(add1 5) "6")
  (check-prog '(add1 -1) "0")
  (check-prog '(sub1 43) "42")
  (check-prog '(sub1 0) "-1"))
