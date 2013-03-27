#lang racket

(define (shift n bits)
  (arithmetic-shift n bits))

(define emit
  (lambda args
    (apply printf args)
    (printf "\n")))

(define fixnum-mask     #b00000011)
(define fixnum-tag      #b00000000)
(define fixnum-shift    2)

(define char-mask       #b11111111)
(define char-tag        #b00001111)
(define char-shift      8)

(define boolean-mask    #b01111111)
(define boolean-tag     #b00011111)
(define boolean-shift   7)

(define empty-list      #b00101111)

(define (emit-header filename)
  (printf #<<END
	.file	"~a"
	.text
	.align 4,0x90
.globl _scheme_entry
_scheme_entry:

END
    filename
))

(define (apply-tag n mask tag)
  (bitwise-ior
    (bitwise-and n (bitwise-not mask))
    (bitwise-and tag mask)))

(define (compile-program x filename output)
  (define (immediate-rep x)
    (cond
      [(integer? x) (apply-tag (shift x fixnum-shift) fixnum-mask fixnum-tag)]
      [(char? x) (apply-tag (shift (char->integer x) char-shift) char-mask char-tag)]
      [(boolean? x) (apply-tag (shift (if x 1 0) boolean-shift) boolean-mask boolean-tag)]
      [(null? x) empty-list]
      [else
        (raise-argument-error 'x "kracket?" x)]))

  (define (emit-assembly)
    (with-output-to-string
      (lambda ()
        (emit-header filename)
        (emit "\tmovl $~a, %eax" (immediate-rep x))
        (emit "\tret"))))

  (let [(assembly (emit-assembly))]
    (with-output-to-file output
      #:exists 'replace
      (lambda () (display assembly)))))

(define (compile-file filename output)
  (compile-program (file->value filename) filename output))

(let* [(args (current-command-line-arguments))
       (input-filename (vector-ref args 0))
       (output-filename (vector-ref args 1))]
    (compile-file input-filename output-filename))
