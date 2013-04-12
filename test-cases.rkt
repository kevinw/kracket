#lang racket

(require
  rackunit
  "compiler.rkt")

(define (check-prog-output program arch expected)
  (let [(tmp-file (path->string (make-temporary-file "~a.s")))]
    (compile-program program "test-program" tmp-file arch)
    (with-check-info
      (['assembly tmp-file]
       ['arch arch]
       ['program (value->string program)])

      (check-not-exn
        (lambda ()
          (check-equal?
            expected
            (let [(exe (link tmp-file arch))]
              (string-trim (with-output-to-string
                (lambda ()
                  (check-true (system exe) "executable did not return 0")))))))))))

(define (check-prog program expected-output)
  (define program-string (with-output-to-string (lambda () (write program))))

  (test-begin
    (check-prog-output program 'x86 expected-output)
    (check-prog-output program 'x86_64 expected-output)))

;(define compiler-tests
  ;(test-suite
    ;"Tests for the compiler"

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
  (check-prog '(zero? 9999)' "#f")
  (check-prog '(zero? #f)' "#f")
  (check-prog '(zero? #t)' "#f")

  (check-prog '(integer? 0) "#t")
  (check-prog '(integer? -1) "#t")
  (check-prog '(integer? 500) "#t")
  (check-prog '(integer? #\c) "#f")
  (check-prog '(integer? #f) "#f")
  (check-prog '(integer? #t) "#f")

  (check-prog '(boolean? #t) "#t")
  (check-prog '(boolean? #f) "#t")
  (check-prog '(boolean? 0) "#f")
  (check-prog '(boolean? -1) "#f")
  (check-prog '(boolean? 500) "#f")
  (check-prog '(boolean? #\c) "#f"))

(test-case
  "Binary primitives"

  (check-prog '(+ 4 9) "13")
  (check-prog '(+ 0 0) "0")

  (check-prog '(- 0 5) "-5")
  (check-prog '(- 100 99) "1")

  (check-prog '(* 3 7) "21")
  (check-prog '(* -2 -3) "6")
  (check-prog '(* -1 1000) "-1000")

  (check-prog '(= 0 1) "#f")
  (check-prog '(= 0 0) "#t")
  (check-prog '(= -500 -500) "#t")

  (check-prog '(< 0 1) "#t")
  (check-prog '(< 1 1) "#f")
  (check-prog '(< 10 10) "#f"))

(test-case
  "Let expressions"

  (check-prog '(let ((a 1)) a) "1")
  (check-prog '(let ((a 2) (b 150)) (* a b)) "300"))

(test-case
  "If expressions"

  (check-prog '(if #t 1 2) "1")
  (check-prog '(if #f 1 2) "2")
  (check-prog '(if #\c
                 (let [(foo #\f)]
                   (if foo 42 100))
                 99)
              "42"))

(test-case
  "Cons"

  (check-prog '(cons 10 20) "(10 . 20)"))
;))

;(require rackunit/text-ui)
;(run-tests compiler-tests 'verbose)
