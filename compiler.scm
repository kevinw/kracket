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

(define (compile-program x filename)
  (define (immediate-rep x)
    (cond
      [(integer? x) (shift x fixnum-shift)]
      [(char? x) (shift x char-shift)]
      [else (error "cannot provide immediate-rep")]))

  (emit-header filename)
  (emit "\tmovl $~a, %eax" (immediate-rep x))
  (emit "\tret"))

(define (compile-file filename)
  (compile-program (file->value filename) filename))

(let [(args (current-command-line-arguments))]
  (if (> (vector-length args) 0)
    (compile-file (vector-ref (current-command-line-arguments) 0))
    (compile-program (read (current-input-port)) "stdin.scm")))
