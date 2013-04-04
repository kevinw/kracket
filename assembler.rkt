#lang racket

(provide mov sub assemble-or shr shl imul
         emit emit-no-tab dest-as-string current-size-suffix)

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
  (define (<name> src dest)
    (emit "~a ~a, ~a"
          (with-size-suffix (symbol->string '<name>))
          (src-as-string src)
          (dest-as-string dest))))

(define (with-size-suffix op)
  (format "~a~a" op (current-size-suffix)))

(defop mov)

(provide add)

; (define (mov src dest)
  ; (emit "~a ~a, ~a" (with-size-suffix "mov") (src-as-string src) (dest-as-string dest)))

(define (assemble-or src dest)
  (emit "~a ~a, ~a" (with-size-suffix "or") (src-as-string src) (dest-as-string dest)))

(define (add src dest)
  (emit "~a ~a, ~a" (with-size-suffix "add") (src-as-string src) (dest-as-string dest)))

(define (sub src dest)
  (emit "~a ~a, ~a" (with-size-suffix "sub") (src-as-string src) (dest-as-string dest)))

(define (shr src dest)
  (emit "~a ~a, ~a" (with-size-suffix "shr") (src-as-string src) (dest-as-string dest)))

(define (shl src dest)
  (emit "~a ~a, ~a" (with-size-suffix "shl") (src-as-string src) (dest-as-string dest)))

(define (imul src dest)
  (emit "~a ~a, ~a" (with-size-suffix "imul") (src-as-string src) (dest-as-string dest)))

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

