#lang racket

;3
(define (findFirst pred lst [noMatch null])
  (cond
    ((null? lst) noMatch)
    ((pred (car lst)) (car lst))
    (else (findFirst pred (cdr lst) noMatch))))

;4
(define (lookup-variable-value var env) ; simple lookup (without frames) -> only one scope
      (define val (findFirst (lambda (x) (eq? (car x) var)) env))
      (if (eq? val false)
          (error "unbound variable" var)
          (cdr val)))


;2
(define (mc-eval exp env)
  (cond ((number? exp) exp)
        ((symbol? exp) (lookup-variable-value exp env))  ;to resolve variables and primitive functions
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        ((pair? exp) (mc-apply (mc-eval (car exp) env) (list-of-values (cdr exp) env))) ; (operation operands..)
        (else (error "Unknown expression type -- EVAL" exp))))

(define (mc-apply procedure arguments)
  (cond ((tagged-list? procedure 'primitive) (apply-primitive-procedure procedure arguments))
        (else (error "Unknown procedure type -- APPLY" procedure))))

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

(define (and? x)
  (tagged-list? x 'and))

(define (or? x)
  (tagged-list? x 'and))

(define (eval-and exp env)
  (and (eq? true (mc-eval (car (cdr exp))))
       (eq? true (mc-eval (car (cdr (cdr exp)))))))

(define (eval-or exp env)
  (or (eq? true (mc-eval (car (cdr exp))))
      (eq? true (mc-eval (car (cdr (cdr exp)))))))

(define myenv (list (cons '* (list 'primitive *))))
(mc-eval '('* '5 '3) myenv)


;5
(define (last l)
  (match l
    ((list a) a)
    ((list) #f)
    ((list a .. b) (last (cdr l)))))

(define (length l)
  (match l
    ((list a ... b) (+ 1 (length (cdr l))))
    ((list a) 1)
    ((list) 0)))