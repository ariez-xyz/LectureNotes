#lang racket
(require racket/trace)

(display "=================================================================\n")
;==============================================================================
; A1

; The primitive evaluator is quite simple:
;   *   The entry point is mc-eval. It takes our expression in form of a tree
;       (aka list aka chained pairs) or a single symbol, as well as an
;       environment where all variable values are stored (that includes function
;       definitions).
;       If the symbol happens to be a number, the symbol will be returned
;       immediately.
;       For all other symbols, we need to look up their current value in our
;       environment using lookup-variable-value.
;       If the expression is in form of a pair, a procedure call is made. Thus,
;       we resolve the procedure name to its value, evaluate all parameters
;       and apply the parameters to the procedure.
;
;   *   mc-apply expects our procedure and already evaluated parameters and calls
;       procedure with our parameters. In primitive, this is done by special
;       variables in the environment, a pair consisting of the symbol 'primitive
;       and the native procedure of our hosting racket implementation.
;
;   *   lookup-variable-value requires a symbol as variable name and our
;       environment.
;       Our environment is a list of assignments. Assignments are simply pairs
;       consisting of the variable name as car and the value as cdr. So,
;       lookup-variable-value searches for the first pair in our env. that has
;       the variable name as car using assq. If found, it returns the value of
;       the variable. If not, we cannot proceed
;
;   *   list-of-values takes the arguments of a procedure call. It evaluates each
;       parameter and puts them in a list, in the same ordering.
;
;   ===========================================================================
;
; The compound evaluator works similar, but has a few changes:
;
;   *   mc-eval recognizes a different type of expression: lambdas
;       mc-eval tags the lambda functions using the 'procedure symbol and builds
;       a new list with the parameter variable names, the procedure body (because
;       its evaluation is delayed until called) and the current environment.
;
;       If a lambda gets called, it is passed as procedure to mc-apply, which
;       recognizes the 'procedure flag and thus calls apply-compound-procedure
;
;       In apply-compound-procedure, the body of the lambda expression will be
;       evaluated, but with a special twist:
;       If we were to directly evaluate a lambda expression that expects
;       parameters, we would run into a unbound variable error sooner or later.
;       Thus, the environment that we pass to the recursive mc-eval call needs
;       to be expanded by another layer of (temporary) variables, which is done
;       by calling extend-environment and consequently make-frame.
;       This implementation also allows nested lambdas as the new layers are
;       just stacked upon. The same identifer may be used multiple times, as the
;       value of the most recent frame will be used.
;       Also, the newly created frame is temporary. As soon as the nested mc-eval
;       call is done, the new frame is no longer reachable and we are back to our
;       original environment.


(display "=================================================================\n")
;==============================================================================
; A2

;towards a metacircular interpreter (racket-interpreter implemented in racket)
;based on SICP interpreter in chapter 4.

;step 1: primitive

;can handle numbers, symbols, primitive procedures; but no compound procedures yet
;there is only the global environment with a single scope; no frames

(define (mc-eval exp env)
  (cond
    [(number? exp) exp]
    [(boolean? exp) exp]                             ; CHANGE: Handle #f and #t
    [(symbol? exp) (lookup-variable-value exp env)]  ;to resolve variables and primitive functions
    [(pair? exp) (mc-apply (mc-eval (car exp) env) (list-of-values (cdr exp) env))] ; (operation operands..)
    [else (error "Unknown expression type -- EVAL" exp)]
  )
)
(provide mc-eval)

(define (mc-apply procedure arguments)
  (cond
    [(tagged-list? procedure 'primitive) (apply-primitive-procedure procedure arguments)]
    [(tagged-list? procedure 'special-syntax)       ; CHANGE: Add special-syntax tag for and/or
        (cond
          [(eq? (cadr procedure) 'and) (andmap identity arguments)]
          [(eq? (cadr procedure) 'or)  (ormap  identity arguments)]
          [else (error "Unknown special syntax type -- APPLY" procedure)]
        )
    ]
    [else (error "Unknown procedure type -- APPLY" procedure)]
  )
)
(provide mc-apply)

(define (lookup-variable-value var env) ; simple lookup (without frames) -> only one scope
  (define val (assq var env))

  (cond
    [(not (eq? val false)) (cdr val)]
    [else
      (define expr-tag (special-expr? var))         ; CHANGE: Handle predefined expression tags
      (if (not (eq? expr-tag false))
          (list expr-tag var)
          (error "unbound variable" var)
      )
    ]
  )
)
(provide lookup-variable-value)

(define (list-of-values exps env)
  (if (null? exps)
      '()
      (cons (mc-eval (car exps) env) (list-of-values (cdr exps) env))
  )
)
(provide list-of-values)

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false)
)
(provide tagged-list?)

(define (special-expr? expr)                        ; CHANGE: predefined expression tags
    (define flags '( ((and or) special-syntax) ))
    (define result (assf (lambda (exprList) (memq expr exprList) ) flags))
    (if (pair? result)
        (cadr result)
        #f
    )
)
(provide special-expr?)

(define (apply-primitive-procedure proc args)  ;e.g. proc: (primitive #<procedure:*>)  args: (1 2)
  (apply-in-underlying-racket (car (cdr proc)) args)
)
(provide apply-primitive-procedure)

(define apply-in-underlying-racket apply) ;redirect to implementation language
(provide apply-in-underlying-racket)

(trace mc-eval)
(trace mc-apply)
(trace lookup-variable-value)
(trace list-of-values)
(trace tagged-list?)
(trace special-expr?)
(trace apply-primitive-procedure)
(trace apply-in-underlying-racket)

; true and false are still not defined by-default and need to be defined in our namespace
; this is because true/false are variables defined as #t/#f, respectively.
; #t/#f are special constants and are handled in mc-eval->boolean?
(define boolns (list (cons 'true true) (cons 'false false)))

(displayln "Native (and true true):") (and true true)
(displayln "Native (and true (and true false)):")(and true (and true false))

(displayln "Evaluator (and true true):") (mc-eval '(and true true) boolns)
(displayln "Evaluator (and true (and true false)):") (mc-eval '(and true (and true false)) boolns)
(displayln "Evaluator (and true true false:") (mc-eval '(and true true false) boolns)



(display "=================================================================\n")
;==============================================================================
; A3

(define (mcc-eval exp env)
  (cond ((number? exp) exp)
        ((symbol? exp) (lookup-variable-value-compound exp env))  ;to resolve variables and primitive functions
        ((lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ((pair? exp) (mcc-apply (mcc-eval (car exp) env) (list-of-values-compound (cdr exp) env))) ; application: (operation operands..)
        (else (error "Unknown expression type -- EVAL" exp))))

(define (mcc-apply procedure arguments)
  (cond ((tagged-list? procedure 'primitive) (apply-primitive-procedure procedure arguments))
        ((tagged-list? procedure 'procedure) (apply-compound-procedure procedure arguments))
        (else (error "Unknown procedure type -- APPLY" procedure))))



(define (lookup-variable-value-compound var env) ;lookup with multiple frames
  (if (null? env)
      (error "unbound variable" var)
      (let ((binding (assq var (car env))))
        (if (eq? binding false)
            (lookup-variable-value-compound var (cdr env))
            (cdr binding)))))

(define (list-of-values-compound exps env)
  (if (null? exps)
      '()
      (cons (mcc-eval (car exps) env) (list-of-values-compound (cdr exps) env))))

; tagged-list? is the same

; apply-primitive-function is the same

; apply-in-underlying-racket is the same

(define (apply-compound-procedure proc args)
  (mcc-eval (caddr proc) (extend-environment (cadr proc) args (cadddr proc))))

(define (lambda? exp) (tagged-list? exp 'lambda)) ;;;for compound procedures (lambda)

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (lambda-parameters exp) (cadr exp))  ; (car (cdr exp))
(define (lambda-body exp) (caddr exp))       ; (car (cdr (cdr exp))

(define (extend-environment vars vals base-env)
  (cons (make-frame vars vals) base-env))

(define (make-frame vars vals)
  (cond ((and (null? vars) (null? vals)) '())
        ((null? vars) (error "too many arguments supplied" vals))
        ((null? vals) (error "too few arguments supplied" vars))
        (else (cons (cons (car vars) (car vals))
                    (make-frame (cdr vars) (cdr vals))))))

; Namespace must be null - no namespace shenanigans
; For primitive this is only possible - if at all - by using function calls
; There is no other way to get recursion in mc-eval, at least without namespace hacks.
; Because we would need a endless stream of calls, we would need an endless list, whose
; construction alone would not allow us to enter mc-eval
;
; In mcc-eval, however we can define recursive lambdas, where mcc-eval is the compound
; version of mc-eval
(define ex '( ((lambda (x) (x x)) (lambda (x) (x x))) )     )
;(mcc-eval ex null)
(provide mcc-eval)


(display "=================================================================\n")
;==============================================================================
; A4

;; Church numeral representation of 0.
(define zero (λ (f) (λ (x) x)))

;; Returns the next Church numeral after n.
(define (succ n)
  (λ (f) (λ (x) (f ((n f) x)))))

;; Church numeral for 1.
(define one (succ zero))

;; Adds two Church numerals.
(define (add n m)
  (λ (f) (λ (x) ((n f) ((m f) x)))))

;; Multiplies two Church numerals.
(define (mult n m)
  (λ (f)
    (n (m f))))

;; Computes n^m with Church numerals.
(define (exp n m)
  (m n))

;; Church encoding of true.
(define churchTrue (λ (x) (λ (y) x)))

;; Church encoding of false.
(define churchFalse zero)

;; Church encoding of the pair (x, y).
(define (make-pair x y)
  (λ (selector) ((selector x) y)))

;; Returns the first element of a pair.
(define (fst p)
  (p churchTrue))

;; Returns the second element of a pair.
(define (snd p)
  (p churchFalse))

;; Returns (x, x+1).
(define (self-and-succ x)
  (make-pair x (succ x)))

;; Given (x, y), returns (y, y+1).
(define (shift p)
  (self-and-succ (snd p)))

;; Returns the predecessor to the Church numeral n by
;; applying n to shift and (0, 0), then taking the first
;; element of the pair.
(define (pred n)
  (fst ((n shift) (make-pair zero zero))))

;; The eager Y combinator.
(define (fix f)
  ((λ (x) (λ (y) ((f (x x)) y)))
      (λ (x) (λ (y) ((f (x x)) y)))
  )
)

;; Returns whether a Church numeral is 0.
(define (is0 n)
  ((n (λ (x) churchFalse)) churchTrue))

;; An "if-then-else" function (the then and else branches
;; must be wrapped in zero-argument lambdas).
(define (ifte c t e)
  (((c t) e)))

;; The factorial function.
(define fact
  (fix (λ (f)
         (λ (n)
           (ifte (is0 n)
                 (λ () one)
                 (λ () (mult n (f (pred n)))))))))

;; Converts a Church numeral to a Racket number.
(define (as-number n)
  ((n add1) 0))

;; Converts a Church boolean to a Racket boolean.
(define (as-bool b)
  ((b #t) #f))


(define (sub m n) (λ (f)
                     (λ (x) ((((n pred) m) f) x))
                  )
)

(define whatTheHeck?
  (fix (λ (f)
          (λ (n)
             (ifte (is0 (sub (succ (succ zero)) n))
                   (lambda () (add (f (pred n)) (f (pred (pred n)))))
                   (lambda () (succ zero))
             )
          )
       )
  )
)
(provide ifte)
(provide is0)
(provide sub)
(provide succ)
(provide pred)
(provide whatTheHeck?)


(display "=================================================================\n")
;==============================================================================
; A5

(define (as-church n)
  (cond
    [(= 0 n) zero]
    [(> 0 n) (error "Negative number:" n)]
    [else (succ (as-church (sub1 n)))]
  )
)
;(trace as-church)
;(trace as-number)

(provide as-church)
(provide as-number)


(displayln "as-church 10:")
(define churchTen (time (as-church 10)))
(displayln "as-church 100:")
(define churchHund (time (as-church 100)))
(displayln "as-church 1'000:")
(define churchThou (time (as-church 1000)))
(displayln "as-church 1'000'000:")
(define churchMil (time (as-church 1000000)))
(displayln "as-church 25'000'000:")
(define churchTFMil (time (as-church 25000000)))

(displayln "as-number 10:")
(time (as-number churchTen))
(displayln "as-number 100:")
(time (as-number churchHund))
(displayln "as-number 1'000:")
(time (as-number churchThou))
(displayln "as-number 1'000'000:")
(time (as-number churchMil))
(displayln "as-number 25'000'000:")
(time (as-number churchTFMil))

; Interestingly enough, this inefficient implementation of as-church needs less
; time than the conversion back to a number.
