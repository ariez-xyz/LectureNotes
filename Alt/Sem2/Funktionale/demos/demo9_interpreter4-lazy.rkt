#lang racket

;towards a metacircular interpreter (racket-interpreter implemented in racket)
;based on SICP interpreter in chapter 4.

;step 4: lazy evaluation

;implementing a normal-order evaluation:
;all primitive procedures are strict (same as in applicative order)
;all compound procedures are non-strict (lazy)

(define (mc-eval exp env)
  (cond ((number? exp) exp)
        ((symbol? exp) (lookup-variable-value exp env))  ;to resolve variables and primitive functions
        ((lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ((if? exp) (eval-if exp env))
        ((pair? exp) (mc-apply (actual-value (car exp) env) (cdr exp) env)) ;lazy
        (else (error "Unknown expression type -- EVAL" exp))))

(define (mc-apply procedure arguments env) ;lazy (added environment parameter; primitive operations are executed strict)
  (cond ((tagged-list? procedure 'primitive) (apply-primitive-procedure procedure (list-of-arg-values arguments env)))
        ((tagged-list? procedure 'procedure) (apply-compound-procedure procedure arguments env))
        (else (error "Unknown procedure type -- APPLY" procedure))))



(define (lookup-variable-value var env) ;lookup with multiple frames
  (if (null? env)
      (error "unbound variable" var)
      (let ((binding (assq var (car env))))
        (if (eq? binding false)
            (lookup-variable-value var (cdr env))
            (cdr binding)))))

(define (list-of-values exps env)
  (if (null? exps)
      '()
      (cons (mc-eval (car exps) env) (list-of-values (cdr exps) env))))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (apply-primitive-procedure proc args)  ;e.g. proc: (primitive #<procedure:*>)  args: (1 2)
  (apply-in-underlying-racket (car (cdr proc)) args))

(define apply-in-underlying-racket apply) ;redirect to implementation language

(define (apply-compound-procedure proc args env) ;lazy
  (mc-eval (caddr proc) (extend-environment (cadr proc) (list-of-delayed-args args env) (cadddr proc))))

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

(define (if? exp) (tagged-list? exp 'if))

(define (eval-if exp env)
  (if (eq? true (mc-eval (cadr exp) env))
      (mc-eval (caddr exp) env)
      (mc-eval (cadddr exp) env)))

(define (actual-value exp env) ;lazy
  (force-it (mc-eval exp env)))

(define (force-it obj) ;lazy
  (if (tagged-list? obj 'thunk)
      (actual-value (cadr obj) (caddr obj))
      obj))

(define (list-of-arg-values exps env) ;lazy
  (if (null? exps) ;no operands
      '()
      (cons (actual-value (car exps) env)
            (list-of-arg-values (cdr exps)
                                env))))

(define (list-of-delayed-args exps env) ;lazy
  (if (null? exps) ;no operands
      '()
      (cons (delay-it (car exps) env)
            (list-of-delayed-args (cdr exps)
                                  env))))

(define (delay-it exp env) ;lazy
  (list 'thunk exp env))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some tests



(define myenv (list (list (cons '+ (list 'primitive +)) (cons '- (list 'primitive -)) (cons '* (list 'primitive *)) (cons '/ (list 'primitive /)) (cons '= (list 'primitive =)) (cons 'true true) (cons 'false false))))

;motivation for lazy
(mc-eval '((lambda (a b) (if (= a 0) 1 b)) 0 (/ 1 0)) myenv) ; for strict evaluation (applicative order) this raises a /: division by zero

