#lang racket

;; Racket's syntax quite simple
;; It is based on S-expressions ("symbolic expressions")
;; to represent code and data
;; S-expressions: nested lists (tree-structure)    --that's the reason for all those ( ..) ((((((...)

;data vs. code


(= 4 (+  2 2))

'(= 4 (+  2 2))
(list '= '4 (list '+  '2 '2))     ;no need to quote numbers

(define mylist (list '= '4 (list '+  '2 '2)))

(length mylist)


(define ns (make-base-namespace)) ;build a namespace

(eval mylist ns)  ;;treat data as code: evaluate it

;

(apply + '(1 2))  ;; apply a function to data



;;;;;;;;;;;;;;;;;;;;;;;;
;;Lookup  (used for our interpreter) (no need to use assq; we could implement this ourselves)
(define myenv (list (cons 'a 0) (cons 'b 1) (cons 'c 2)))
(assq 'c myenv)
(assq 'x myenv)

(define (lookup var env)
      (define val (assq var env))
      (if (eq? val false)
          (error "unbound variable" var)
          (cdr val)))

(lookup 'c myenv)

