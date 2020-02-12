#lang racket
;---------------
;1) powerCloseTo
(define (powerCloseTo b n)
  (define (iteratePowers b n e)
    (if (> (expt b e) n)
        e
        (iteratePowers b n (+ e 1))))
  (iteratePowers b n 1))

;---------------
;2) iterative fib + performance
(define (fib n)
  (define (fibIter a1 a2 n max)
    (if (>= n max)
        (+ a1 a2)
        (fibIter (+ a1 a2) a1 (+ n 1) max)))
  
  (if (= n 0)
      0
      (fibIter 1 0 2 n)))

(define (fibTree n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fibTree (- n 1)) (fibTree (- n 2))))))

;traces
(require racket/trace)
(display "traces\n")
(trace fib)
(trace fibTree)
(fib 5)
(fibTree 5)
(untrace fib)
(untrace fibTree)

;ms
(define ms (current-inexact-milliseconds))
(display "time for F_30\n")
(fib 30)
(display "iterative ms: ")
(- (current-inexact-milliseconds) ms)
(define ms1 (current-inexact-milliseconds))
(fibTree 30)
(display "tree recursion ms: ")
(- (current-inexact-milliseconds) ms1)

;---------------
;3) myLoop
(define (myLoop f n)
  (when (> n 0)
    f
    myLoop f (- n 1)))

;---------------
;4a) min-fx-gx
(define (min-fx-gx f g x)
  (min (f x) (g x)))

;---------------
;4b) combine-fx-gx
(define (combine-fx-gx func f g x)
  (func (f x) (g x)))

;---------------
;5) explanations
(define (f g)
  (g 5))
(define (square x) (* x x))

(f square)
(f (lambda (x) (* x (+ x 2))))
;(f f)

#| Explanations:
1. 25, calls (square 5)
2. 35, 5 * (5 + 2)
3. error, calls (f 5), 5 is not a procedure
|#

;---------------
;6) iterative sum
(define (sum term a next b)
  (define (iter a result)
    (if (>= a b)
        (+ (term a) result)
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(sum (lambda (x) x) 1 (lambda (x) (+ x 1)) 100)
(sum (lambda (x) (* x x)) 25 (lambda (x) (+ x 1)) 26)

;---------------
;7) examples + better good-enough
(define (sqrt-iter guess x)
  (if (better-good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x  guess)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (average x y)
  (/ (+ x y) 2))

;will return ~0.03 for positive values smaller than 0.0001 (meaning (sqrt 0.0001) ~= (sqrt 0.00000001))
(define (good-enough? guess x)
  (< (abs (- (* guess guess) x)) 0.001))

;working with relative accuracy allows for better estimations (meaning (sqrt 0.0001) != (sqrt 0.00000001))
(define (better-good-enough? guess x)
  (< (abs (- 1 (/ (* guess guess) x))) 0.001))