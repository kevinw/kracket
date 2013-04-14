#lang racket

(provide assemble offset offset-as-string dest-as-string src-as-string current-size-suffix)

(define current-size-suffix (make-parameter "l"))

(struct label-obj (name) #:transparent)
(struct op (operand) #:transparent)

(struct offset (bytes register))

(define (offset-as-string offset)
  (format "~a(%~a)" (offset-bytes offset) (offset-register offset)))

(define (dest-as-string dest)
  (cond
    [(offset? dest) (offset-as-string dest)]
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

(define (op->string op)
  (cond
    [(op? op) (symbol->string (op-operand op))]
    [(symbol? op)(format "~a~a" op (current-size-suffix))]
    [else (error "op->string given unexpected value" op)]))

(define-syntax-rule (defop <name>)
  (begin
    (provide <name>)
    (define (<name> src dest)
      (list '<name> src dest))))

(defop mov)
(defop add)
(defop sub)
(defop shr)
(defop shl)
(defop imul)
(defop cmp)
(defop sal)

(define (or! src dest) (list 'or src dest))
(provide or!)

(define (je label) (list (op 'je) label))
(provide je)

(define (jmp label) (list (op 'jmp) label))
(provide jmp)

(define (label label) (list (label-obj label)))
(provide label)

(define (ret) (list (op 'ret)))
(provide ret)

(define (and! src dest) (list 'and src dest))
(provide and!)

(define (sete dest) (list (op 'sete) dest))
(provide sete)

(define (setl dest) (list (op 'setl) dest))
(provide setl)

(define (push src) (list 'push src))
(provide push)

(define (pop dest) (list 'pop dest))
(provide pop)

(define (assemble-line source-line)
  ; Turn an assembly operation list like
  ;  (list 'mov 5 'eax) into "movl $5, %eax"
  (define f (first source-line))
  (if (label-obj? f)
    (format "~a:" (label-obj-name f))
    (let* ([op-str (op->string f)]
           [code (case (length source-line)
             [(3) (format "~a ~a, ~a" op-str
                    (src-as-string (second source-line))
                    (dest-as-string (third source-line)))]
             [(2) (format "~a ~a" op-str (dest-as-string (second source-line)))]
             [(1) (format "~a" op-str)]
             [else (error "source line expected to have 1, 2, or 3 elems" source-line)])])

      (string-append "\t" code))))

(define (operation? l)
  (let ([f (first l)])
    (or (symbol? f) (op? f) (label-obj? f))))

(define (assemble-flatten l)
  (cond
    [(or (void? l) (empty? l)) '()]
    [(operation? l) (list l)]
    [else (append (assemble-flatten (first l)) (assemble-flatten (rest l)))]))

(define (assemble sources)
  (let ([flattened-src (assemble-flatten sources)])
    (string-join (map assemble-line flattened-src) "\n")))

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

