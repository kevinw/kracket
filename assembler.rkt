#lang racket

(provide emit emit-no-tab dest-as-string current-size-suffix)

(define current-size-suffix (make-parameter "l"))

(define emit-no-tab
  (lambda args
    (apply printf args)
    (printf "\n")))

(define emit
  (lambda args
    (printf "\t")
    (apply emit-no-tab args)))

(define (dest-as-string dest)
  (cond
    [(string? dest) dest]
    [(symbol? dest) (format "%~a" (symbol->string dest))]
    [else (error "unknown target" dest)]))

(define (src-as-string src)
  (cond
    [(integer? src) (format "$~a" src)]
    [else (dest-as-string src)]))

(define size-suffixes
  '((32, "q")
    (64, "l")))

(define-syntax-rule (defop <name>)
  (begin
      (provide <name>)
      (define (<name> src dest)
        (emit "~a ~a, ~a"
              (with-size-suffix (symbol->string '<name>))
              (src-as-string src)
              (dest-as-string dest)))))

(define (with-size-suffix op)
  (format "~a~a" op (current-size-suffix)))

(defop mov)
(defop add)
(defop sub)
(defop shr)
(defop shl)
(defop imul)
(defop cmp)
(defop sal)

(define (or! src dest) (emit "~a ~a, ~a" (with-size-suffix "or") (src-as-string src) (dest-as-string dest)))
(provide or!)

(define (je label) (emit "je ~a" label))
(provide je)

(define (jmp label) (emit "jmp ~a" label))
(provide jmp)

(define (label label) (emit-no-tab "~a:" label))
(provide label)

(define (ret) (emit "ret"))
(provide ret)

(define (and! src dest) (emit "~a ~a, ~a" (with-size-suffix "and") (src-as-string src) (dest-as-string dest)))
(provide and!)

(define (sete dest) (emit "sete ~a" (dest-as-string dest)))
(provide sete)

(define (setl dest) (emit "setl ~a" (dest-as-string dest)))
(provide setl)

#|
(define-syntax-rule (mov src dest)
  (let [(src* (src-as-string src))
        (dest* (dest-as-string dest))]
    (emit "mov ~a, ~a" src* dest*)))

(require (for-syntax syntax/boundmap
                     syntax/parse))
(begin-for-syntax 
  (define const-table (make-free-identifier-mapping)))
  
(define-syntax (defconst stx)
  (syntax-parse stx
    [(_ name:id v:integer)
     #'(begin
         (define-syntax name 
           (lambda (stx)
             (cond [(identifier? stx)
                    #'v]
                   [else
                    (raise-syntax-error #f "Can't apply the integer constant" stx)])))
         (begin-for-syntax
           (free-identifier-mapping-put! const-table #'name #'v)))]))
  
 
(define-syntax (peek stx)
  (syntax-parse stx
    [(_ name:id)
     (begin
       (define compile-time-value
         (free-identifier-mapping-get const-table #'name (lambda () #'#f)))
       
       (printf "At compile time, I see ~s is ~s\n" #'name compile-time-value)
       #'name)]))
 
(defconst x 42)
(peek x)
|#

