#lang racket

;==============================================================================
; A1


(display "=================================================================\n")
;==============================================================================
; A2

;step 2: compound

;can handle also compound procedures (added lambda)
;models the environment model (with multiple frames) -> changed lookup and environment

(define (mc-eval exp env [flags '()])
  (cond
    [(number? exp) exp]
    [(boolean? exp) exp]
    [(string? exp) exp]
    [(symbol? exp) (lookup-variable-value exp env)]  ;to resolve variables and primitive functions
    [(lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env)]
    [(logicalExpr? exp) (apply-logical (logical-type exp) (logical-operands exp) env flags)]
    [(pair? exp) (mc-apply (mc-eval (car exp) env flags) (list-of-values (cdr exp) env flags) flags)] ; application: (operation operands..)
    [else (error "Unknown expression type -- EVAL" exp)]
  )
)

(define (mc-apply procedure arguments flags)
  (cond
    [(tagged-list? procedure 'primitive) (apply-primitive-procedure procedure arguments flags)]
    [(tagged-list? procedure 'procedure) (apply-compound-procedure  procedure arguments flags)]
    [else (error "Unknown procedure type -- APPLY" procedure)]
  )
)



(define (lookup-variable-value var env) ;lookup with multiple frames
  (define lookup-pred (lambda (x) (eq? var (car x))))
  (if (null? env)
      (error "unbound variable" var)
      (let [(binding (findFirst lookup-pred (car env) '__UNBOUND__))];(assq var (car env)))]
        (if (eq? binding '__UNBOUND__)
            (lookup-variable-value var (cdr env))
            (cdr binding)
        )
      )
  )
)

(define (list-of-values exps env flags)
  (if (null? exps)
      '()
      (cons (mc-eval (car exps) env flags) (list-of-values (cdr exps) env flags))
  )
)

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false)
)

(define (apply-primitive-procedure proc args flags)  ;e.g. proc: (primitive #<procedure:*>)  args: (1 2)
  (apply-in-underlying-racket (car (cdr proc)) args)
)

(define apply-in-underlying-racket apply) ;redirect to implementation language

(define (apply-compound-procedure proc args flags)
  (mc-eval (caddr proc) (extend-environment (cadr proc) args (cadddr proc)) flags)
)

(define (lambda? exp) (tagged-list? exp 'lambda)) ;;;for compound procedures (lambda)

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (lambda-parameters exp) (cadr exp))  ; (car (cdr exp))
(define (lambda-body exp) (caddr exp))       ; (car (cdr (cdr exp))

(define (extend-environment vars vals base-env)
  (cons (make-frame vars vals) base-env))

(define (make-frame vars vals)
  (cond
    [(and (null? vars) (null? vals)) '()]
    [(null? vars) (error "too many arguments supplied" vals)]
    [(null? vals) (error "too few arguments supplied" vars)]
    [else (cons (cons (car vars) (car vals))
                    (make-frame (cdr vars) (cdr vals)))]
  )
)

; -----------------------------------------------
; LOGICAL OPERATORS

(define (logicalExpr? exp)
  (or (eq? 'or  (logical-type exp))
      (eq? 'and (logical-type exp))
  )
)
(define (apply-logical type operands env flags)
  (if (lookup-flag 'short-circuit flags)
      (apply-logical-short    type operands env flags)
      (apply-logical-nonshort type operands env flags)
  )
)
(define (apply-logical-short type operands env flags)
  (define curried-eval (curryr mc-eval env flags))
  (cond
    [(eq? 'and type) (andmap curried-eval operands)]
    [(eq? 'or  type) (ormap  curried-eval operands)]
    [else (error "unknown logical operator" type " - APPLY-LOGICAL")]
  )
)
(define (apply-logical-nonshort type operands env flags)
  ((cond
     [(eq? 'or  type) ormap]
     [(eq? 'and type) andmap]
     [else (error "unknown logical operator" type "-APPLY-LOGICAL")]
   )

   (lambda (x) x)
   (list-of-values operands env flags)
  )
)
(define logical-type car)
(define logical-operands cdr)

; -----------------------------------------------
; EVALUATOR FLAGS

(define default-eval-flags
  (list
    (cons 'short-circuit #t)
  )
)
(define (lookup-flag flag lst)
  (define lookup-pred (lambda (x) (eq? flag (car x))))
  (let ([el (findFirst lookup-pred lst '__INVALID_FLAG__)])
    (if (eq? el '__INVALID_FLAG__)
        (let ([el (findFirst lookup-pred default-eval-flags '__INVALID_FLAG__)])
          (if (eq? el '__INVALID_FLAG__)
              (error "invalid flag:" flag "- LOOKUP-FLAG")
              (cdr el)
          )
        )
        (cdr el)
    )
  )
)

; -----------------------------------------------
; FINDFIRST

; Tail recursion to the rescue!
(define (findFirstRecursive pred lst [noMatch #f])
  (cond
    [(null? lst) noMatch]
    [(pred (car lst)) (car lst)]
    [else (findFirstRecursive pred (cdr lst) noMatch)]
  )
)

; Another way to get the first occurrence is to utilize pattern matching
(define (findFirstPattern pred lst [noMatch #f])
  (match lst
    [(list (not (? pred a)) ... (? pred a) _ ...) a]
    [_ noMatch]
  )
)

(provide findFirstRecursive)
(provide findFirstPattern)

(define findFirst findFirstPattern)
; -----------------------------------------------

(require racket/trace)
;(trace mc-eval)
;(trace mc-apply)
;(trace list-of-values)
;
;(trace logicalExpr?)
;(trace apply-logical)
;(trace apply-logical-short)
;(trace apply-logical-nonshort)
;(trace lookup-flag)


(provide mc-eval)
(provide lookup-flag)


; The implementation is straight-forward:
;   - mc-eval has a new optional parameter "flags" which is a list of pairs where each pair
;     contains the flag name as symbol and the value of the flag
;   - Flags allow to alter the evaluator's behaviour, like enabling/disabling short-circuit
;     evaluation
;   - Flags are looked up using procedure lookup-flag. If mc-eval's flags does not contain that
;     specific flag, it is looked up in the default flags.
;
;   - mc-eval has been extended to recognize the special forms 'and and 'or using procedure
;     logicalExpr?. If the expression is a valid logical expression, mc-eval calls apply-logical.
;   - apply-logical then checks the flag 'short-circuit and calls procedure apply-logical-short
;     when 'short-circuit is #t and apply-logical-nonshort otherwise.
;   - apply-logical-short utilizes andmap/ormap with a right-curried mc-eval as (misused)
;     predicate and the list of operands.
;     The procedure inherits short-circuit-behaviour from andmap/ormap and the lazy
;     evaluation by the predicate procedure.
;   - apply-logical-nonshort utilizes andmap/ormap with an identity function as (misused)
;     predicate and an evaluated list of operands.
;     Altough andmap/ormap is short-circuit, the operands have been evaluated before they were
;     passed to those procedures. Thus, we are short-circuiting the non-short-circuited operands
;     which leads to correct behaviour and a performance gain when having a lot of operands.

; There are two ways to check the evaluation mode:
;   - Using trace
;   - Calling and and or with parameters such that the second one shall not be considered
;     in short-circuit mode. e.g. (and #f ((lambda () (displayln "This is not short-circuit!") #t)))


(define myns (list (list (cons 'displayln (list 'primitive displayln)))))

(displayln "Testing mc-eval in short-circuit mode:")
(mc-eval '(and #t #f ((lambda () (displayln "This is not short-circuit: and!") #t)) #t) myns)
(mc-eval '(or  #f #t ((lambda () (displayln "This is not short-circuit!: or!") #f)) #t) myns)

(displayln "Testing mc-eval in non-short-circuit mode:")
(define flags '( (short-circuit . #f) ))
(mc-eval '(and #t #f ((lambda () (displayln "This is not short-circuit: and!") #t)) #t) myns flags)
(mc-eval '(or  #f #t ((lambda () (displayln "This is not short-circuit!: or!") #f)) #t) myns flags)



(display "=================================================================\n")
;==============================================================================
; A3

; The procedure is integrated into A2
;(trace findFirst)
(provide findFirst)


(define (stream->listn str n)
    (cond
          [(= 0 n) '()]
              [(stream-empty? str) '()]
                  [else (append (list (stream-first str)) (stream->listn (stream-rest str) (sub1 n)))]
                    )
    )
(provide stream->listn)

(define bigListSize 1000000)
(define bigList (stream->listn (in-naturals) bigListSize))
(displayln (~a "Time for findFirstRecursive with n=" bigListSize))
(time (findFirstRecursive (λ (x) (= x (sub1 bigListSize))) bigList))
(displayln (~a "Time for findFirstPattern with n=" bigListSize))
(time (findFirstPattern (λ (x) (= x (sub1 bigListSize))) bigList))

; According to tests it seems that pattern matching beats tail recursion with
; "cpu time: 0" vs "cpu time: 1" for element sizes < 100'000.
; However, with n=1'000'000, tail recursion is drastically faster than pattern
; matching with "cpu time: 8" vs. "cpu time: 116"


(display "=================================================================\n")
;==============================================================================
; A4

; This change was already performed above for interpreter2

;step 1: primitive

;can handle numbers, symbols, primitive procedures; but no compound procedures yet
;there is only the global environment with a single scope; no frames

(define (mc-eval-primitive exp env)
  (cond ((number? exp) exp)
        ((symbol? exp) (lookup-variable-value-primitive exp env))  ;to resolve variables and primitive functions
        ((pair? exp) (mc-apply-primitive (mc-eval-primitive (car exp) env) (list-of-values-primitive (cdr exp) env))) ; (operation operands..)
        (else (error "Unknown expression type -- EVAL" exp))))

(define (mc-apply-primitive procedure arguments)
  (cond ((tagged-list? procedure 'primitive) (apply-primitive-procedure-primitive procedure arguments))
        (else (error "Unknown procedure type -- APPLY" procedure))))



(define (lookup-variable-value-primitive var env) ; simple lookup (without frames) -> only one scope
      (define val (findFirst (lambda (x) (eq? var (car x))) env '__UNBOUND__))
      (if (eq? val '__UNBOUND__)
          (error "unbound variable" var)
          (cdr val)))

(define (list-of-values-primitive exps env)
  (if (null? exps)
      '()
      (cons (mc-eval-primitive (car exps) env) (list-of-values-primitive (cdr exps) env))))

; tagged-list? is the same as in compound

(define (apply-primitive-procedure-primitive proc args)  ;e.g. proc: (primitive #<procedure:*>)  args: (1 2)
  (apply-in-underlying-racket (car (cdr proc)) args))

; apply-in-underlying-racket is the same as in compound



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some tests


;self evaluating
(mc-eval-primitive 6 null) ;6
(mc-eval-primitive '6 null) ;6

;(mc-eval-primitive 'a null)
(mc-eval-primitive 'b (list (cons 'b 20)))


;--> application
(tagged-list? (cons 'primitive (cons 4 5)) 'primitive) ;#t
(apply-primitive-procedure-primitive (list 'primitive *) (list 1 2 3)) ;6

;define an environment that knows about the * operation, and perform a multiplication
(define myenv (list (cons '* (list 'primitive *))))
(mc-eval-primitive '(* 5 3) myenv)
(mc-eval-primitive '(* 5 (* 2 4)) myenv)

;define an environment that also contains a variable
(define myenv2 (list (cons '* (list 'primitive *)) (cons 'a  100)))
(mc-eval-primitive '(* 5 (* 2 a)) myenv2)

;(require racket/trace)
;(trace mc-eval-primitive)
;(trace mc-apply-primitive)

(list-of-values-primitive (list 'a '* 2 (list '* 2 'a)) myenv2)


(display "=================================================================\n")
;==============================================================================
; A5
(define (last lst)
  (match lst
    [(list a ... b) b]
    [(list) (void)]
    [(cons a b) b]
    [_ (error "unknown parameter" lst)]
  )
)
(provide last)

(last '(1 2 3 4 5 6 7 8 9 10))
(last '(a b))
(last '(#t))
(last '())
(last (cons 1 2))

(define (mylength lst [cnt 0])
  (match lst
    [(list) cnt]
    [(list _) (add1 cnt)]
    [(list _ rest ...) (mylength rest (add1 cnt))]
  )
)
(provide mylength)

(mylength '(1 2 3 4 5 6 7 8 9 10))
(mylength '(a b))
(mylength '(#t))
(mylength '())
