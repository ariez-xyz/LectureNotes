#lang racket

;towards a metacircular interpreter (racket-interpreter implemented in racket)
;based on SICP interpreter in chapter 4.

;step 3: if-special-case 

;added "if" 

(define (mc-eval exp env)
  (cond ((number? exp) exp)
        ((symbol? exp) (lookup-variable-value exp env))  ;to resolve variables and primitive functions
        ((lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ((if? exp) (eval-if exp env))
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

(define (if? exp) (tagged-list? exp 'if))

(define (eval-if exp env)
  (if (eq? true (mc-eval (cadr exp) env))
      (mc-eval (caddr exp) env)
      (mc-eval (cadddr exp) env)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some tests



(define myenv (list (list (cons '+ (list 'primitive +)) (cons '- (list 'primitive -)) (cons '* (list 'primitive *)) (cons '/ (list 'primitive /)) (cons '= (list 'primitive =)) (cons 'true true) (cons 'false false))))




(mc-eval '(if true 1 2) myenv)
(mc-eval '((lambda (x) (if true x 0)) 3) myenv)











;recursive function
(mc-eval '((lambda (n)
   ((lambda (fact)
      (fact fact n))
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1)))))))
 5) myenv)





;find out if our if is a special case (short-circuit evaluation)

;remember:
(define (try a b)
  (if (= a 0) 1 b ))
;(try 0 55)
;(try 0 (/ 1 0))




(mc-eval '(if (= 1 1) 1 (/ 1 0)) myenv)
(mc-eval '((lambda () (if (= 1 1) 1 (/ 1 0)) )) myenv) ;same within lambda


#|(define (eval-if exp env)
  (define then (mc-eval (caddr exp) env))
  (define else (mc-eval (cadddr exp) env))
  (if (eq? true (mc-eval (cadr exp) env))
      then
      else))
|#
;note comment out the recursive function above








;what is the result of this? with / without the short-circuit version
;(mc-eval '((lambda (a b) (if (= a 0) 1 b)) 0 (/ 1 0)) myenv) 
















;motivation for lazy
;(mc-eval '((lambda (a b) (if (= a 0) 1 b)) 0 (/ 1 0)) myenv) ; for strict evaluation (applicative order) this raises a /: division by zero

