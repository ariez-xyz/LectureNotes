#lang racket

;towards a metacircular interpreter (racket-interpreter implemented in racket)
;based on SICP interpreter in chapter 4.

;step 2: compound 

;can handle also compound procedures (added lambda)
;models the environment model (with multiple frames) -> changed lookup and environment

(define (mc-eval exp env)
  (cond ((number? exp) exp)
        ((symbol? exp) (lookup-variable-value exp env))  ;to resolve variables and primitive functions
        ((lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ((pair? exp) (mc-apply (mc-eval (car exp) env) (list-of-values (cdr exp) env))) ; application: (operation operands..)
        (else (error "Unknown expression type -- EVAL" exp))))

(define (mc-apply procedure arguments)
  (cond ((tagged-list? procedure 'primitive) (apply-primitive-procedure procedure arguments))
        ((tagged-list? procedure 'procedure) (apply-compound-procedure procedure arguments))
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

(define (apply-compound-procedure proc args)
  (mc-eval (caddr proc) (extend-environment (cadr proc) args (cadddr proc))))

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some tests



(define myenv (list (list (cons '* (list 'primitive *)))))
(define myenv2 (list (list (cons '* (list 'primitive *)) (cons 'a  100))))

(mc-eval '((lambda () 3)) myenv)

(mc-eval '((lambda (x y) (* x y)) 2 3) myenv)
(mc-eval '(* 5 ((lambda (x) (* x x)) 3)) myenv)
(mc-eval '(* 5 ((lambda (x) (* x x)) 3) ((lambda (y) (* y y)) 2)) myenv)

(mc-eval '(* a ((lambda (a) (* a a)) 3)) myenv2)


;
(define myenv3 (list (list (cons 'append (list 'primitive append)) (cons 'list (list 'primitive list)) (cons 'a  100))))
(mc-eval '(append (list 1 2) (list 3 4)) myenv3)

;a procedure declaration is nothing else than a shortcut with lambda
(define (square x) (* x x))
(define mysquare (lambda (x) (* x x)))

;bool
(define myenv5 (list (list (cons '+ (list 'primitive +)) (cons '- (list 'primitive -)) (cons '* (list 'primitive *)) (cons '/ (list 'primitive /)) (cons '= (list 'primitive =)) (cons 'true true) (cons 'false false))))
(mc-eval '((lambda (x) true) 3) myenv5)

(mc-eval '((lambda (x) 21) 3) myenv5)
