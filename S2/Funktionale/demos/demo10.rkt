#lang racket


;;Memoization for arbitrary single-argument functions
(define (memoize fn)
  (define cache (make-hash))
    (lambda (arg) (hash-ref! cache arg (λ() (fn arg)))))

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(set! fib (memoize fib)) ;;replace fib by the memoized fib function

(time (fib 100))

;this does not work with functions that have e.g., 2 arguments. -- see a solution below
(define (plus a b)
  (sleep 2)
  (+ a b))

(set! plus (memoize plus))
;(plus 3 4)  ;; error: arity mismatch  (see below for a solution to this)





;;;;;; jfyi: ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;jfyi: var-args
(define (myPlus . x)
  (apply + x))

(myPlus 1 2 3 4)


(define (myPrint . xs)
  (for-each displayln xs))

(myPrint 1 2 3 4)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
;;jfyi: Memoization for functions with any number of arguments
;;      note the differences:  1)  lambda (arg)   vs.    lambda arg
;;                             2)  (fn arg)       vs.    (apply fn arg)

(define (memoize fn)
  (let ((cache (make-hash)))
    (lambda arg (hash-ref! cache arg (λ() (apply fn arg))))))

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))
(set! fib (memoize fib)) ;;replace fib by the memoized fib function

(time (fib 100))

(define (plus a b)
  (sleep 2)
  (+ a b))

(set! plus (memoize plus))
(plus 3 4)
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
