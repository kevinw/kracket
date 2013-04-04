#lang racket

(require racket/system)

(require "assembler.rkt")

(define word-size 8)
(define eax 'eax)
(define rax 'rax)
(define stack-register 'rsp)
(define (stack-ptr index)
  (format "~a(%~a)" index stack-register))

(define shift arithmetic-shift)

(define (value->string x)
  (with-output-to-string
    (lambda () (write x))))

(define fixnum-mask     #b00000011)
(define fixnum-tag      #b00000000)
(define fixnum-shift    2)

(define char-mask       #b11111111)
(define char-tag        #b00001111)
(define char-shift      8)

(define boolean-mask    #b01111111)
(define boolean-tag     #b00111111)
(define boolean-shift   7)

(define empty-list      #b00101111)

(define heap-mask       #b00000111)

(define pair-tag        #b00000001)
(define vector-tag      #b00000010)
(define string-tag      #b00000011)
(define symbol-tag      #b00000101)
(define closure-tag     #b00000110)

(define (emit-header filename)
	; .file	"~a"
  (printf #<<END
	.text
	.align 4,0x90
.globl _scheme_entry
_scheme_entry:

END
    ; filename
))

(define (apply-tag n mask tag)
  (bitwise-ior
    (bitwise-and n (bitwise-not mask))
    (bitwise-and tag mask)))

(define (immediate-rep x)
  (cond
    [(integer? x) (apply-tag (shift x fixnum-shift) fixnum-mask fixnum-tag)]
    [(char? x) (apply-tag (shift (char->integer x) char-shift) char-mask char-tag)]
    [(boolean? x) (apply-tag (shift (if x 1 0) boolean-shift) boolean-mask boolean-tag)]
    [(null? x) empty-list]
    [else (raise-argument-error 'x "don't know how to provide an immediate-rep" x)]))

(define (primcall? x)
  (member (first x)
    '(add1 sub1 integer->char char->integer zero? integer? boolean? + - * = <)))

(define (primcall-op x) (first x))
(define (primcall-operand1 x) (second x))
(define (primcall-operand2 x) (third x))

(define (immediate? x)
  (for/or
      ([pred (list integer? char? boolean? null?)])
    (pred x)))

(define (variable? x) (symbol? x))
(define (lookup x env) (hash-ref env x))

(define (if? x) (eq? (first x) 'if))
(define (if-test x) (second x))
(define (if-conseq x) (third x))
(define (if-altern x) (fourth x))

(define (prep-binary-call x si env)
  (emit-expr (primcall-operand2 x) si env)
  (emit "mov %rax, ~a" (stack-ptr si))
  (emit-expr
    (primcall-operand1 x)
    (- si word-size)
    env))

(define unique-label
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      (format "label~a" count))))

(define (emit-cmpl constant reg)
  (emit "cmpl $~a, %~a" constant (symbol->string reg)))

(define (emit-je label) (emit "je ~a" label))
(define (emit-jmp label) (emit "jmp ~a" label))
(define (emit-label label)
  (emit-no-tab "~a:" label))

(define (emit-if test conseq altern si env)
  (let [(L0 (unique-label)) (L1 (unique-label))]
    (emit-expr test si env)
    (emit-cmpl (immediate-rep #f) eax)
    (emit-je L0)
    (emit-expr conseq si env)
    (emit-jmp L1)
    (emit-label L0)
    (emit-expr altern si env)
    (emit-label L1)))

(define (emit-primitive-call x si env)
  (case (primcall-op x)
    [(+)
     (prep-binary-call x si env)
     (add (stack-ptr si) rax)]
    [(-)
     (prep-binary-call x si env)
     (sub (stack-ptr si) rax)]
    [(*)
     (prep-binary-call x si env)
     (shr fixnum-shift rax)
     (shr fixnum-shift (stack-ptr si))
     (imul (stack-ptr si) rax)
     (shl fixnum-shift rax)]
    [(=)
     (prep-binary-call x si env)
     (emit "cmp ~a, %rax" (stack-ptr si))
     (emit "mov $0, %rax")
     (emit "sete %al")
     (emit "sal $~a, %rax" boolean-shift)
     (assemble-or boolean-tag rax)]
     ;(emit "or $~a, %rax" boolean-tag)]
    [(<)
     (prep-binary-call x si env)
     (emit "cmp ~a, %rax" (stack-ptr si))
     (emit "mov $0, %rax")
     (emit "setl %al")
     (emit "sal $~a, %rax" boolean-shift)
     (emit "or $~a, %rax" boolean-tag)]
    [(add1)
     (emit-expr (primcall-operand1 x) si env)
     (emit "add $~a, %rax" (immediate-rep 1))]
    [(sub1)
     (emit-expr (primcall-operand1 x) si env)
     (emit "subq $~a, %rax" (immediate-rep 1))]
    [(integer->char)
     (emit-expr (primcall-operand1 x) si env)
     (emit "shl $~a, %rax" (- char-shift fixnum-shift))
     (emit "or $~a, %rax" char-tag)]
    [(char->integer)
     (emit-expr (primcall-operand1 x) si env)
     (emit "shr $~a, %rax" (- char-shift fixnum-shift))]
    [(zero?)
     (emit-expr (primcall-operand1 x) si env)
     (emit "cmp $0, %rax")
     (emit "mov $0, %rax")
     (emit "sete %al")
     (emit "sal $~a, %rax" boolean-shift)
     (emit "or $~a, %rax" boolean-tag)]
    [(integer?)
     (emit-expr (primcall-operand1 x) si env)
     (emit "and $~a, %rax" fixnum-mask)
     (emit "cmp $~a, %rax" fixnum-tag)
     (emit "mov $0, %rax")
     (emit "sete %al")
     (emit "sal $~a, %rax" boolean-shift)
     (emit "or $~a, %rax" boolean-tag)]
    [(boolean?)
     (emit-expr (primcall-operand1 x) si env)
     (emit "and $~a, %rax" boolean-mask)
     (emit "cmp $~a, %rax" boolean-tag)
     (emit "mov $0, %rax")
     (emit "sete %al")
     (emit "sal $~a, %rax" boolean-shift)
     (emit "or $~a, %rax" boolean-tag)]))

(define (let? x) (eq? (first x) 'let))
(define (bindings x) (second x))
(define (body x) (third x))

(define (lhs binding) (first binding))
(define (rhs binding) (second binding))

(define (make-env) (hash))
(define (extend-env variable-name stack-index env)
  (hash-set env variable-name stack-index))

(define (emit-let bindings body si env)
  (let f [(b* bindings) (new-env env) (si si)]
    (cond
      [(null? b*) (emit-expr body si new-env)]
      [else
        (let [(b (first b*))]
          (emit-expr (rhs b) si env)
          (emit "mov %rax, ~a" (stack-ptr si))
          (f (rest b*)
             (extend-env (lhs b) si new-env)
             (- si word-size)))])))

(define heap-register 'rdi)
(define (heap-ptr offset)
  (format "~a(%~a)" offset (symbol->string heap-register)))

(define (emit-cons head tail si env)
  (emit-expr head si env (heap-ptr 0))
  (emit-expr tail si env (heap-ptr word-size))
  (mov heap-register rax)
  (or pair-tag rax)
  (emit "add $~a, %~a" (* 2 word-size) heap-register))

(define (cons-call? x) (eq? (first x) 'cons))
(define (cons-head x) (first (rest x)))
(define (cons-tail x) (second (rest x)))

(define (emit-expr x si env [dest rax])
  (cond
    [(immediate? x)
     (emit "movq $~a, ~a" (immediate-rep x) (dest-as-string dest))]
    [(variable? x)
     (emit "mov ~a, %rax" (stack-ptr (lookup x env)))]
    [(let? x)
     (emit-let (bindings x) (body x) si env)]
    [(if? x)
     (emit-if (if-test x) (if-conseq x) (if-altern x) si env)]
    [(primcall? x)
     (emit-primitive-call x si env)]
    [(cons-call? x)
     (emit-cons (cons-head x) (cons-tail x) si env)]
    [else
      (error (format "don't know how to emit expression \"~a\"" (value->string x)))]))

(define size-suffixes
  '((x86 "l")
    (x86_64 "q")))

(define (assemble-sources expr filename architecture)
  (let ([stack-index (- word-size)]
        [env (hash)])
    (emit-header filename)
    (emit-expr expr stack-index env)
    (emit "ret")))

(define (compile-program x filename output)
  (define (emit-assembly)
    (with-output-to-string
      (lambda ()
        (let ([size-suffix (second (assoc 'x86_64 size-suffixes))])
            (parameterize ([current-size-suffix size-suffix])
                (assemble-sources x filename 'x86))))))

  (let [(assembly (emit-assembly))]
    (with-output-to-file output
      #:exists 'replace
      (lambda () (display assembly)))))

(define (compile-file filename output)
  (compile-program (file->value filename) filename output))

(define (system-check cmd)
  (when (not (system cmd))
    (error (format "command exited with error: ~a" cmd))))

(define (link assembly)
  (let [(tmp-file (path->string (make-temporary-file)))]
    (system-check (format "gcc -O3 driver.c aux.c ~a -o ~a" assembly tmp-file))
    tmp-file))

(define (compile-and-exec program filename)
  (let [(tmp-file (path->string (make-temporary-file "~a.s")))]
    (compile-program program filename tmp-file)
    (let [(exe (link tmp-file))]
      (string-trim (with-output-to-string
        (lambda ()
          (system-check exe)))))))

(let [(args (current-command-line-arguments))]
  (when (> (vector-length args) 0)
    (let [(input-filename (vector-ref args 0))
          (output-filename (vector-ref args 1))]
       (compile-file input-filename output-filename))))

(provide compile-and-exec compile-file link immediate? emit-expr primcall?)
