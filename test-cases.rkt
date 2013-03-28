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

(test-case
  "Primitive conversions"

  (check-prog '(integer->char 65) "A")
  (check-prog '(integer->char 97) "a")
  (check-prog '(integer->char 0) "\u0000")
  (check-prog '(char->integer #\a) "97")
  (check-prog '(char->integer #\F) "70")
  (check-prog '(char->integer #\u0000) "0"))

(test-case
  "Primitive predicates"

  (check-prog '(zero? 0)' "#t")
  (check-prog '(zero? -1)' "#f")
  (check-prog '(zero? 1)' "#f")
  (check-prog '(zero? 9999)' "#f"))
