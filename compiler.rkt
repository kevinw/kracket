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

(define default-arch "x86_64")

(require "assembler.rkt")

(struct arch (name size-suffix word-size scratch-register scratch-2-register stack-register heap-register str-src-reg str-dest-reg str-count-reg gcc-arch) #:transparent)
(define architectures
  (list
    (arch "x86"    "l" 4 'eax 'ebx 'esp 'esi 'esi 'edi 'ecx "i386")
    (arch "x86_64" "q" 8 'rax 'rbx 'rsp 'rsi 'rsi 'rdi 'rcx "x86_64")))

(define archs-by-name
  (make-immutable-hash
    (for/list ([arch architectures])
      (cons (arch-name arch) arch))))

(define arch-param (make-parameter (hash-ref archs-by-name default-arch)))

(define (arch-parameterize arch-name cb)
  (let ([arch (hash-ref archs-by-name arch-name)])
    (parameterize ([arch-param arch]
                   [current-size-suffix (arch-size-suffix arch)])
      (cb))))

; define syntax shortcuts for accessing parameters like they were just identifiers
(define-syntax-rule (define-param-id <id> <struct-getter>)
  (define-syntax <id>
    (syntax-id-rules ()
      [<id> (<struct-getter> (arch-param))])))

(define-param-id word-size arch-word-size)
(define-param-id scratch arch-scratch-register)
(define-param-id scratch-2 arch-scratch-2-register)
(define-param-id stack-register arch-stack-register)
(define-param-id heap-register arch-heap-register)
(define-param-id str-src-reg arch-str-src-reg)
(define-param-id str-dest-reg arch-str-dest-reg)
(define-param-id str-count-reg arch-str-count-reg)

(define (compile-cmd input output arch)
  ; Returns the GCC command to link together a complete progam.
  (define gcc-arch-flag (arch-gcc-arch (hash-ref archs-by-name arch)))
  (define CFLAGS (format "-std=c99 -Wall -g -O3 -arch ~a" gcc-arch-flag)) ; TODO: parse this from the Makefile? emit the makefile?
  (define LFLAGS "-Wl,-no_pie")

  (format "gcc ~a ~a driver.c aux.c ~a -o ~a" CFLAGS LFLAGS input output))

(define-syntax dword-size
  (syntax-id-rules ()
    [dword-size (* word-size 2)]))

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

(define (immediate? x)
  (for/or
      ([pred (list integer? char? boolean? empty-list-quote?)])
    (pred x)))

(define (immediate-rep x)
  ; Return the integer constant that represents x
  (cond
    [(integer? x) (apply-tag (shift x fixnum-shift) fixnum-mask fixnum-tag)]
    [(char? x) (apply-tag (shift (char->integer x) char-shift) char-mask char-tag)]
    [(boolean? x) (apply-tag (shift (if x 1 0) boolean-shift) boolean-mask boolean-tag)]
    [(empty-list-quote? x) empty-list]
    [else (raise-argument-error 'x "don't know how to provide an immediate-rep" x)]))

(define (primcall? x)
  (and (list? x)
    (member (first x)
      '(add1 sub1 integer->char char->integer zero? integer? boolean?
        pair? + - * = < car cdr make-vector vector-length vector-ref vector?))))

(define (primcall-op x) (first x))
(define (primcall-operand1 x) (second x))
(define (primcall-operand2 x) (third x))

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
    (sete 'al)
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
     (sete 'al)
     (sal boolean-shift scratch)
     (or! boolean-tag scratch))]
    [(<) (list
     (prep-binary-call x si env)
     (cmp (stack-ptr si) scratch)
     (mov 0 scratch)
     (setl 'al)
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
     (sete 'al)
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
     (emit-vector x si env))]
    [(vector-length) (list
      (emit-vector-length x si env))]
    [(vector-ref) (list
      (emit-vector-ref x si env))]))

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

(define (emit-vector-length x si env)
  (list
    (emit-expr (primcall-operand1 x) si env)
    (fail-if-not-type vector-tag heap-mask)
    (and! (bitwise-not heap-mask) scratch)
    (mov (offset 0 scratch) scratch)))

(define (emit-vector-ref x si env)
  (list
    (emit-expr (primcall-operand1 x) si env) ; vector
    (emit-expr (primcall-operand2 x) si env scratch-2) ; length fixnum
    (shr fixnum-shift scratch-2)
    (and! (bitwise-not heap-mask) scratch)
    (mov (format "(%~a, %~a, 1)" scratch scratch-2) scratch)))

(define (fail-if-not-type tag mask)
  (void))

(define (data-ref label-name)
  (if (equal? (arch-name (arch-param)) "x86")
    (format "~a" label-name)
    (format "~a(%rip)" label-name)))

(define (emit-string x si env)
  (define utf8-bytes (string->bytes/utf-8 x))
  (define str-length (bytes-length utf8-bytes))
  (define label (unique-label))
  (list 
    (mov (+ word-size str-length) scratch-2)
    (mov str-length (offset 0 heap-register))

    (push heap-register)

    (mov heap-register str-dest-reg) ; destination
    (add word-size str-dest-reg)

    (data label utf8-bytes)
    (lea (data-ref label) str-src-reg)

    (mov str-length str-count-reg)

    (cld)
    (rep movsb)

    (pop heap-register)

    ; tag string pointer
    (mov heap-register scratch) 
    (or! string-tag scratch)

    (align-to-dword scratch-2)
    (add scratch-2 heap-register)
    ))

(define (let? x) (and (list? x) (eq? (first x) 'let)))

(define (labels? x)
  (and
    (list? x)
    (equal? (first x) 'labels)))

; For the labels form, a new set of unique labels are created
; and the initial environment is constructed to map each of 
; the lvars to its corresponding label.
;
; <Prog>  ::= (labels ((lvar <LExpr>) ...) <Expr>)
;
(define (emit-labels x si env)
  (unless (equal? (first x) 'labels)
    (error "expected labels expression" x))
  (unless (eq? (length x) 3)
    (error "syntax error: expected (label ([lvar ...]) expr)"))

  (let ([lvars (second x)]
        [expr (third x)])
    (emit-lvars lvars si env)))

(define (emit-lvars x si env)
  (cond
    [(empty? x) null]
    [else (map (lambda (var) (emit-lvar var si env)) x)]))

(define (emit-lvar x si env)
  (let ([label-name (first x)]
        [code (second x)])
    (unless (symbol? label-name)
      (error "expected symbol" label-name))
    (unless (code? code)
      (error "expected (code ...) expr" code))))

(define (first-symbol-eq? l symbol)
  (and
    (list? l)
    (equal? (first l) symbol)))

(define (labelcall? code)
  (first-symbol-eq? code 'labelcall))

(define (code? code)
  (first-symbol-eq? code 'code))

; For each code expression, the label is first emitted, followed
; by the code of the body. The environment used for the body
; contains, in addition to the lvars, a mapping of each of the
; formal parameters to the first set of stack locations (−4, −8,
; etc.). The stack index used for evaluating the body starts above
; the last index used for the formals.
;
; <LExpr> ::= (code (var ...) <Expr>)
;
(define (emit-code x si env)
  #f)

; For a (labelcall lvar e ...), the arguments are evaluated and
; their values are saved in consecutive stack locations, skip-
; ping one location to be used for the return-point. Once all
; of the arguments are evaluated, the value of the stack-pointer,
; %esp is incremented to point to one word below the return-point.
; A call to the label associated with the lvar is issued. A call
; instruction decrements the value of %esp by 4 and saves the ad-
; dress of the next instruction in the appropriate return-point
; slot. Once the called procedure returns (with a value in %eax),
; the stack pointer is adjusted back to its initial position.
(define (emit-labelcall x si env)
  (unless (equal? (first x) 'labelcall) (error "expected 'labelcall" x))
  (let ([label-name (second x)]
        [args (list-tail x 2)])
    (unless (symbol? label-name) (error "expected label-name to be symbol" x))
    #f))

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
      [(labels? x)
       (emit-labels x si env)]
      [(labelcall? x)
       (emit-labelcall x si env)]
      [else
        (raise-argument-error 'x "don't know how to emit" x)])))

(define (empty-list-quote? x)
  (equal? x (quote (quote ()))))

(define (setup-stack-frame proc)
  (define (arg n) ; 32 bit
    (offset (+ word-size (* word-size n)) 'ebp))
  (if (equal? (arch-name (arch-param)) "x86")
    (list
      (push 'ebp)
      (mov 'esp 'ebp)
      (push 'ebx)
      (push 'esi)
      (mov (arg 1) 'esi) ; first param is heap pointer passed into scheme_entry
      (proc)
      (pop 'esi)
      (pop 'ebx)
      (pop 'ebp))
    (list
      (push 'rbx)
      (unless (equal? 'rdi heap-register) ; first arg is pointer to heap; move to our heap-register
        (mov 'rdi heap-register))
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

; read filename, output filename, arch from the command-line
(let [(args (current-command-line-arguments))]
  (when (> (vector-length args) 0)
    (let ([input-filename (vector-ref args 0)]
          [output-filename (vector-ref args 1)]
          [arch (if (> (vector-length args) 2) (vector-ref args 2) default-arch)])
       (compile-file input-filename output-filename arch))))
