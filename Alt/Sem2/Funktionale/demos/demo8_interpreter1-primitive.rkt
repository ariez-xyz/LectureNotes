#lang racket

;towards a metacircular interpreter (racket-interpreter implemented in racket)
;based on SICP interpreter in chapter 4.

;step 1: primitive 

;can handle numbers, symbols, primitive procedures; but no compound procedures yet
;there is only the global environment with a single scope; no frames

(define (mc-eval exp env)
  (cond ((number? exp) exp)
        ((symbol? exp) (lookup-variable-value exp env))  ;to resolve variables and primitive functions
        ((pair? exp) (mc-apply (mc-eval (car exp) env) (list-of-values (cdr exp) env))) ; (operation operands..)
        (else (error "Unknown expression type -- EVAL" exp))))

(define (mc-apply procedure arguments)
  (cond ((tagged-list? procedure 'primitive) (apply-primitive-procedure procedure arguments))
        (else (error "Unknown procedure type -- APPLY" procedure))))



(define (lookup-variable-value var env) ; simple lookup (without frames) -> only one scope
      (define val (assq var env))
      (if (eq? val false)
          (error "unbound variable" var)
          (cdr val)))

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some tests


;self evaluating
(mc-eval 6 null) ;6
(mc-eval '6 null) ;6

;(mc-eval 'a null)
(mc-eval 'b (list (cons 'b 20)))


;--> application
(tagged-list? (cons 'primitive (cons 4 5)) 'primitive) ;#t
(apply-primitive-procedure (list 'primitive *) (list 1 2 3)) ;6

;define an environment that knows about the * operation, and perform a multiplication
(define myenv (list (cons '* (list 'primitive *))))
(mc-eval '(* 5 3) myenv)
(mc-eval '(* 5 (* 2 4)) myenv)

;define an environment that also contains a variable
(define myenv2 (list (cons '* (list 'primitive *)) (cons 'a  100)))
(mc-eval '(* 5 (* 2 a)) myenv2)

;(require racket/trace)
;(trace mc-eval)
;(trace mc-apply)

(list-of-values (list 'a '* 2 (list '* 2 'a)) myenv2)

;(mc-eval '(* 5 (+ 2 a 4)) myenv2)   ;error (no plus defined in myenv2
(mc-eval '(+ 5 (+ 2 a 4)) (cons (cons '+ (list 'primitive +)) myenv2))  ;so we extend our environment by the + operation

