#lang racket

(require
  rackunit
  "compiler.rkt"
  "assembler.rkt")

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
              (with-check-info
                (['exe exe])

                (string-trim (with-output-to-string
                  (lambda ()
                    (check-true (system exe) "executable did not return 0"))))))))))))

(define (check-prog program expected-output)
  (define program-string (with-output-to-string (lambda () (write program))))

  (test-begin
    (check-prog-output program 'x86 expected-output)
    (check-prog-output program 'x86_64 expected-output)))

(define (test-progs name test-cases)
  (test-case name
    (for ([test-case test-cases])
      (unless (eq? (length test-case) 2)
        (error "expected test case to have format (program expected-test-case)"))
      (let ([program (first test-case)]
            [expected-output (value->string (second test-case))])
        (check-prog program expected-output)))))

(test-progs
  "Primitives"

  '((5 5)
    (5439 5439)
    (#\a #\a)
    (#\Z #\Z)))

(test-progs
  "Unary expressions"

  '(((add1 5) 6)
    ((add1 -1) 0)
    ((sub1 43) 42)
    ((sub1 0) -1)))

(test-progs
  "Primitive conversions"

  '(((integer->char 65) #\A)
    ((integer->char 97) #\a)
    ((integer->char 0) #\nul)
    ((char->integer #\a) 97)
    ((char->integer #\F) 70)
    ((char->integer #\u0000) 0)))

(test-progs
  "Primitive predicates"

  '(((zero? 0) #t)
    ((zero? -1) #f)
    ((zero? 1) #f)
    ((zero? 9999) #f)
    ((zero? #f) #f)
    ((zero? #t) #f)

    ((integer? 0) #t)
    ((integer? -1) #t)
    ((integer? 500) #t)
    ((integer? #\c) #f)
    ((integer? #f) #f)
    ((integer? #t) #f)

    ((boolean? #t) #t)
    ((boolean? #f) #t)
    ((boolean? 0) #f)
    ((boolean? -1) #f)
    ((boolean? 500) #f)
    ((boolean? #\c) #f)))

(test-progs
  "Binary primitives"

  '(((+ 4 9) 13)
    ((+ 0 0) 0)

    ((- 0 5) -5)
    ((- 100 99) 1)

    ((* 3 7) 21)
    ((* -2 -3) 6)
    ((* -1 1000) -1000)

    ((= 0 1) #f)
    ((= 0 0) #t)
    ((= -500 -500) #t)

    ((< 0 1) #t)
    ((< 1 1) #f)
    ((< 10 10) #f)))

(test-progs
  "Let expressions"

  '(((let ((a 1)) a) 1)
    ((let ((a 2) (b 150)) (* a b)) 300)))

(test-progs
  "If expressions"

  '(((if #t 1 2) 1)
    ((if #f 1 2) 2)
    ((if #\c
      (let [(foo #\f)]
        (if foo 42 100))
          99)
      42)))

(test-progs
  "Cons"

  '(((cons 10 20) (10 . 20))
    ((cons 10 (cons 20 30)) (10 20 . 30))))

(test-progs
  "Car/Cdr"

  '(((car (cons 10 20)) 10)
    ((cdr (cons 10 20)) 20)
    ((pair? (cons 1 2)) #t)
    ((pair? 1) #f)
    ((pair? #\c) #f)))

(test-progs
  "Vectors"

  '(((make-vector 1) #(0))
    ((vector? (make-vector 1)) #t)
    ((vector? #f) #f)
    ((vector? 5) #f)

    ((vector-length (make-vector 30)) 30)))

(test-progs
  "Strings"

  '(("hello" "hello")
    ("" "")))

;))

;(require rackunit/text-ui)
;(run-tests compiler-tests 'verbose)
