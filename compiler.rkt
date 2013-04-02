#lang racket

(require racket/system)

(define wordsize 4)
(define eax 'eax)
(define stack-register 'rsp)
(define (stack-ptr index)
  (format "~a(%~a)" index stack-register))

(define shift arithmetic-shift)

(define (value->string x)
  (with-output-to-string
    (lambda () (write x))))

(define emit-no-tab
  (lambda args
    (apply printf args)
    (printf "\n")))

(define emit
  (lambda args
    (printf "\t")
    (apply emit-no-tab args)))

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

(define (immediate-rep x)
  (cond
    [(integer? x) (apply-tag (shift x fixnum-shift) fixnum-mask fixnum-tag)]
    [(char? x) (apply-tag (shift (char->integer x) char-shift) char-mask char-tag)]
    [(boolean? x) (apply-tag (shift (if x 1 0) boolean-shift) boolean-mask boolean-tag)]
    [(null? x) empty-list]
    [else
      (raise-argument-error 'x "kracket?" x)]))

(define (primcall? x)
  (member (first x)
    '(add1 sub1 integer->char char->integer zero? integer? boolean? + - * = <)))

(define (primcall-op x)
  (first x))

(define (primcall-operand1 x)
  (first (rest x)))

(define (primcall-operand2 x)
  (first (rest (rest x))))

(define (immediate? x)
  (for/or
      ([pred (list integer? char? boolean? null?)])
    (pred x)))

(define (variable? x)
  (symbol? x))

(define (lookup x env)
  (hash-ref env x))

(define (if? x)
  (eq? (first x) 'if))

(define (if-test x) (second x))
(define (if-conseq x) (third x))
(define (if-altern x) (fourth x))

(define (prep-binary-call x si env)
  (emit-expr (primcall-operand2 x) si env)
  (emit "movl %eax, ~a" (stack-ptr si))
  (emit-expr
    (primcall-operand1 x)
    (- si wordsize)
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
     (emit "addl ~a, %eax" (stack-ptr si))]
    [(-)
     (prep-binary-call x si env)
     (emit "subl ~a, %eax" (stack-ptr si))]
    [(*)
     (prep-binary-call x si env)
     (emit "shrl $~a, %eax" fixnum-shift)
     (emit "shrl $~a, ~a" fixnum-shift (stack-ptr si))
     (emit "imull ~a, %eax" (stack-ptr si))
     (emit "shl $~a, %eax" fixnum-shift)]
    [(=)
     (prep-binary-call x si env)
     (emit "cmpl ~a, %eax" (stack-ptr si))
     (emit "movl $0, %eax")
     (emit "sete %al")
     (emit "sall $~a, %eax" boolean-shift)
     (emit "orl $~a, %eax" boolean-tag)]
    [(<)
     (prep-binary-call x si env)
     (emit "cmpl ~a, %eax" (stack-ptr si))
     (emit "movl $0, %eax")
     (emit "setl %al")
     (emit "sall $~a, %eax" boolean-shift)
     (emit "orl $~a, %eax" boolean-tag)]
    [(add1)
     (emit-expr (primcall-operand1 x) si env)
     (emit "addl $~a, %eax" (immediate-rep 1))]
    [(sub1)
     (emit-expr (primcall-operand1 x) si env)
     (emit "subl $~a, %eax" (immediate-rep 1))]
    [(integer->char)
     (emit-expr (primcall-operand1 x) si env)
     (emit "shl $~a, %eax" (- char-shift fixnum-shift))
     (emit "or $~a, %eax" char-tag)]
    [(char->integer)
     (emit-expr (primcall-operand1 x) si env)
     (emit "shr $~a, %eax" (- char-shift fixnum-shift))]
    [(zero?)
     (emit-expr (primcall-operand1 x) si env)
     (emit "cmpl $0, %eax")
     (emit "movl $0, %eax")
     (emit "sete %al")
     (emit "sall $~a, %eax" boolean-shift)
     (emit "orl $~a, %eax" boolean-tag)]
    [(integer?)
     (emit-expr (primcall-operand1 x) si env)
     (emit "andl $~a, %eax" fixnum-mask)
     (emit "cmpl $~a, %eax" fixnum-tag)
     (emit "movl $0, %eax")
     (emit "sete %al")
     (emit "sall $~a, %eax" boolean-shift)
     (emit "orl $~a, %eax" boolean-tag)]
    [(boolean?)
     (emit-expr (primcall-operand1 x) si env)
     (emit "andl $~a, %eax" boolean-mask)
     (emit "cmpl $~a, %eax" boolean-tag)
     (emit "movl $0, %eax")
     (emit "sete %al")
     (emit "sall $~a, %eax" boolean-shift)
     (emit "orl $~a, %eax" boolean-tag)]))

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
          (emit "movl %eax, ~a(%rsp)" si)
          (f (rest b*)
             (extend-env (lhs b) si new-env)
             (- si wordsize)))])))
  
(define (emit-expr x si env)
  (cond
    [(immediate? x)
     (emit "movl $~a, %eax" (immediate-rep x))]
    [(variable? x)
     (emit "movl ~a, %eax" (stack-ptr (lookup x env)))]
    [(let? x)
     (emit-let (bindings x) (body x) si env)]
    [(if? x)
     (emit-if (if-test x) (if-conseq x) (if-altern x) si env)]
    [(primcall? x)
     (emit-primitive-call x si env)]
    [else
      (error (format "don't know how to emit expression \"~a\"" (value->string x)))]))

(define (apply-tag n mask tag)
  (bitwise-ior
    (bitwise-and n (bitwise-not mask))
    (bitwise-and tag mask)))

(define (compile-program x filename output)
  (define (emit-assembly)
    (with-output-to-string
      (lambda ()
        (emit-header filename)
        (emit-expr x -4 (hash))
        (emit "ret"))))

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
