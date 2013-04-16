#lang racket

(require racket/system)

(provide assemble-sources
         value->string
         system-check
         default-arch
         compile-program
         compile-file
         link
         immediate?
         emit-expr
         primcall?)

(define default-arch 'x86_64)

(require "assembler.rkt")

; TODO: make define-registers

(define esi 'esi)
(define esp 'esp)
(define ebp 'ebp)
(define edi 'edi)
(define eax 'eax)
(define rax 'rax)
(define rsp 'rsp)
(define al  'al)

(struct arch (name size-suffix word-size scratch-register scratch-2-register stack-register heap-register gcc-arch) #:transparent)
(define architectures
  (list
    (list 'x86    (arch "x86"    "l" 4 'eax 'ebx 'esp 'esi "i386"))
    (list 'x86_64 (arch "x86_64" "q" 8 'rax 'rbx 'rsp 'rdi "x86_64"))))

(define current-arch-param (make-parameter default-arch))
(define word-size-param (make-parameter 8))
(define scratch-register (make-parameter rax))
(define scratch-2-register (make-parameter 'rbx))
(define stack-register-param (make-parameter rsp))
(define heap-register-param (make-parameter 'rdi))

(define (arch-parameterize arch cb)
  (let ([arch (second (assoc arch architectures))])
    (parameterize ([current-arch-param arch]
                   [current-size-suffix (arch-size-suffix arch)]
                   [word-size-param (arch-word-size arch)]
                   [scratch-register (arch-scratch-register arch)]
                   [scratch-2-register (arch-scratch-2-register arch)]
                   [stack-register-param (arch-stack-register arch)]
                   [heap-register-param (arch-heap-register arch)])
      (cb))))

(define (compile-cmd input output arch)
  ; Returns the GCC command to link together a complete progam.
  (define gcc-arch-flag (arch-gcc-arch (second (assoc arch architectures))))
  (define CFLAGS (format "-std=c99 -Wall -g -O3 -arch ~a" gcc-arch-flag)) ; TODO: parse this from the Makefile? emit the makefile?

  (format "gcc ~a driver.c aux.c ~a -o ~a" CFLAGS input output))

; define syntax shortcuts for accessing parameters like they were just identifiers
(define-syntax-rule (define-param-id <id> <param>)
  (define-syntax <id>
    (syntax-id-rules ()
      [<id> (<param>)])))

(define-syntax dword-size
  (syntax-id-rules ()
    [dword-size (* word-size 2)]))

(define-param-id current-arch current-arch-param)
(define-param-id word-size word-size-param)
(define-param-id scratch scratch-register)
(define-param-id scratch-2 scratch-2-register)
(define-param-id stack-register stack-register-param)
(define-param-id heap-register heap-register-param)

(define (stack-ptr index) (format "~a(%~a)" index stack-register))

(define shift arithmetic-shift)

(define (value->string x)
  (with-output-to-string
    (lambda () (write x))))

; Tag objects in memory based on their type

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

(define (apply-tag n mask tag)
  ; Take a number n and apply ~mask & tag
  (bitwise-ior
    (bitwise-and n (bitwise-not mask))
    (bitwise-and tag mask)))

(define (immediate-rep x)
  ; Return the integer constant that represents x
  (cond
    [(integer? x) (apply-tag (shift x fixnum-shift) fixnum-mask fixnum-tag)]
    [(char? x) (apply-tag (shift (char->integer x) char-shift) char-mask char-tag)]
    [(boolean? x) (apply-tag (shift (if x 1 0) boolean-shift) boolean-mask boolean-tag)]
    [(null? x) empty-list]
    [else (raise-argument-error 'x "don't know how to provide an immediate-rep" x)]))

(define (primcall? x)
  (and (list? x)
    (member (first x)
      '(add1 sub1 integer->char char->integer zero? integer? boolean?
        pair? + - * = < car cdr make-vector vector?))))

(define (primcall-op x) (first x))
(define (primcall-operand1 x) (second x))
(define (primcall-operand2 x) (third x))

(define (immediate? x)
  (for/or
      ([pred (list integer? char? boolean? null?)])
    (pred x)))

(define (variable? x) (symbol? x))
(define (lookup x env) (hash-ref env x))

(define (if? x) (and (list? x) (eq? (first x) 'if)))
(define (if-test x) (second x))
(define (if-conseq x) (third x))
(define (if-altern x) (fourth x))

(define (prep-binary-call x si env)
  (list
    (emit-expr (primcall-operand2 x) si env)
    (mov scratch (stack-ptr si))
    (emit-expr (primcall-operand1 x) (- si word-size) env)))

(define unique-label
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      (format "label~a" count))))

(define (emit-if test conseq altern si env)
  (let ([L0 (unique-label)]
        [L1 (unique-label)])
    (list
      (emit-expr test si env)
      (cmp (immediate-rep #f) scratch)
      (je L0)
      (emit-expr conseq si env)
      (jmp L1)
      (label L0)
      (emit-expr altern si env)
      (label L1))))

(define (emit-type-check mask tag)
  (list
    (and! mask scratch)
    (cmp tag scratch)
    (mov 0 scratch)
    (sete al)
    (sal boolean-shift scratch)
    (or! boolean-tag scratch)))

(define (emit-primitive-call x si env)
  (case (primcall-op x)
    [(+) (list
     (prep-binary-call x si env)
     (add (stack-ptr si) scratch))]
    [(-) (list
     (prep-binary-call x si env)
     (sub (stack-ptr si) scratch))]
    [(*) (list
     (prep-binary-call x si env)
     (shr fixnum-shift scratch)
     (shr fixnum-shift (stack-ptr si))
     (imul (stack-ptr si) scratch)
     (shl fixnum-shift scratch))]
    [(=) (list
     (prep-binary-call x si env)
     (cmp (stack-ptr si) scratch)
     (mov 0 scratch)
     (sete al)
     (sal boolean-shift scratch)
     (or! boolean-tag scratch))]
    [(<) (list
     (prep-binary-call x si env)
     (cmp (stack-ptr si) scratch)
     (mov 0 scratch)
     (setl al)
     (sal boolean-shift scratch)
     (or! boolean-tag scratch))]
    [(add1) (list
     (emit-expr (primcall-operand1 x) si env)
     (add (immediate-rep 1) scratch))]
    [(sub1) (list
     (emit-expr (primcall-operand1 x) si env)
     (sub (immediate-rep 1) scratch))]
    [(integer->char) (list
     (emit-expr (primcall-operand1 x) si env)
     (shl (- char-shift fixnum-shift) scratch)
     (or! char-tag scratch))]
    [(char->integer) (list
     (emit-expr (primcall-operand1 x) si env)
     (shr (- char-shift fixnum-shift) scratch))]
    [(zero?) (list
     (emit-expr (primcall-operand1 x) si env)
     (cmp 0 scratch)
     (mov 0 scratch)
     (sete al)
     (sal boolean-shift scratch)
     (or! boolean-tag scratch))]
    [(integer?) (list
     (emit-expr (primcall-operand1 x) si env)
     (emit-type-check fixnum-mask fixnum-tag))]
    [(boolean?) (list
     (emit-expr (primcall-operand1 x) si env)
     (emit-type-check boolean-mask boolean-tag))]
    [(pair?) (list
     (emit-expr (primcall-operand1 x) si env)
     (emit-type-check heap-mask pair-tag))]
    [(vector?) (list
     (emit-expr (primcall-operand1 x) si env)
     (emit-type-check heap-mask vector-tag))]
    [(car) (list
     (emit-expr (primcall-operand1 x) si env)
     (mov (offset -1 scratch) scratch))]
    [(cdr) (list
     (emit-expr (primcall-operand1 x) si env)
     (mov (offset (- word-size 1) scratch) scratch))]
    [(make-vector) (list
     (emit-vector x si env))]))

; todo: memcpy, duh
(define (zerofill n dest)
  (unless (zero? n)
    (let ([zero-addr (* dword-size n)])
      (list 
        (mov 0 (offset zero-addr dest))
        (zerofill (sub1 n) dest)))))

; the DWORD "offset" trick
;   new offset = (offset + align - 1) & ~(align - 1)
(define (align-to-dword dest)
  (let ([align dword-size])
    (list
      (add (sub1 align) dest)
      (and! (- (sub1 align)) dest))))

(define (emit-vector x si env)
  (define vec-length (primcall-operand1 x))
  (list
    (emit-expr vec-length si env) ; length
    (mov scratch (offset 0 heap-register))   ; set the length
    (mov scratch scratch-2)                  ; save the length
    (zerofill vec-length heap-register)
    (mov heap-register scratch)              ; scratch = heap | vector-tag
    (or! vector-tag scratch)
    (align-to-dword scratch-2)
    (add scratch-2 heap-register)))

(define (emit-string x si env)
  (define utf8-bytes (string->bytes/utf-8 x))
  (define str-length (bytes-length utf8-bytes))
  (define label (unique-label))
  (list 
    (mov (+ word-size str-length) scratch-2)
    (mov str-length (offset 0 heap-register))

    (push esi)

    (mov esi edi) ; destination: edi
    (add word-size edi)

    (data label utf8-bytes)
    (mov (format "$~a" label) esi)

    (mov str-length 'ecx)

    (cld)
    (rep movsb)

    (pop esi)

    ; tag string pointer
    (mov heap-register scratch) 
    (or! string-tag scratch)

    (align-to-dword scratch-2)
    (add scratch-2 heap-register)
    ))

(define (let? x) (and (list? x) (eq? (first x) 'let)))

(define (make-env) (hash))
(define (extend-env variable-name stack-index env)
  (hash-set env variable-name stack-index))

(define (emit-let bindings body si env)
  (define (lhs binding) (first binding))
  (define (rhs binding) (second binding))
  (let f [(b* bindings) (new-env env) (si si)]
    (cond
      [(null? b*) (emit-expr body si new-env)]
      [else
        (let [(b (first b*))]
          (list
            (emit-expr (rhs b) si env)
            (mov scratch (stack-ptr si))
            (f (rest b*)
               (extend-env (lhs b) si new-env)
               (- si word-size))))])))

(define (emit-cons head tail si env)
  (list
    (emit-expr head si env)
    (push scratch)
    (emit-expr tail si env)
    (mov scratch (offset word-size heap-register))
    (pop (offset 0 heap-register))
    (mov heap-register scratch)
    (or! pair-tag scratch)
    (add dword-size heap-register)))

(define (cons-call? x) (and (list? x) (eq? (first x) 'cons)))
(define (cons-head x) (first (rest x)))
(define (cons-tail x) (second (rest x)))

(define (emit-expr x si env [dest #f])
  (define (bindings x) (second x))
  (define (body x) (third x))

  (let ([dest (or dest scratch)])
    (cond
      [(immediate? x)
       (mov (immediate-rep x) dest)]
      [(variable? x)
       (mov (stack-ptr (lookup x env)) scratch)]
      [(let? x)
       (emit-let (bindings x) (body x) si env)]
      [(if? x)
       (emit-if (if-test x) (if-conseq x) (if-altern x) si env)]
      [(primcall? x)
       (emit-primitive-call x si env)]
      [(cons-call? x)
       (emit-cons (cons-head x) (cons-tail x) si env)]
      [(string? x)
       (emit-string x si env)]
      [else
        (error (format "don't know how to emit expression \"~a\"" (value->string x)))])))

(define (setup-stack-frame proc)
  (define (arg n) ; 32 bit
    (offset (+ word-size (* word-size n)) ebp))
  (if (equal? (arch-name current-arch) "x86")
    (list
      ; todo make a macro for this ala (preserve (ebx esi) proc)
      (push ebp)
      (mov esp ebp)
      (push 'ebx)
      (push esi)
      (mov (arg 1) esi) ; first param is heap pointer passed into scheme_entry
      (proc)
      (pop esi)
      (pop 'ebx)
      (pop ebp))
    (list
      (push 'rbx)
      (proc)
      (pop 'rbx))))

(define (assemble-sources expr filename)
  (let ([stack-index (- word-size)]
        [env (hash)])
    (assemble (list
      (setup-stack-frame
        (lambda ()
          (emit-expr expr stack-index env)))
      (ret)))))

(define (compile-program x filename output [arch default-arch])
  (define (emit-assembly)
    (arch-parameterize arch
      (lambda ()
        (assemble-sources x filename))))

  (let [(assembly (emit-assembly))]
    (with-output-to-file output
      #:exists 'replace
      (lambda () (display assembly)))))

(define (compile-file filename output arch)
  (compile-program (file->value filename) filename output arch))

(define (system-check cmd)
  (when (not (system cmd))
    (error (format "command exited with error: ~a" cmd))))

(define (link assembly arch)
  (let [(tmp-file (path->string (make-temporary-file)))]
    (system-check (compile-cmd assembly tmp-file arch))
    tmp-file))

(let [(args (current-command-line-arguments))]
  (when (> (vector-length args) 0)
    (let ([input-filename (vector-ref args 0)]
          [output-filename (vector-ref args 1)]
          [arch (if (> (vector-length args) 2) (string->symbol (vector-ref args 2)) default-arch)])
       (compile-file input-filename output-filename arch))))

