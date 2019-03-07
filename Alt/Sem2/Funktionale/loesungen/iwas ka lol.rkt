#lang racket



;1
(define (powerCloseTo b n)
  (define (iter b n e)
    (if (> (expt b e) n)
        e
        (iter b n (+ e 1))))
  (iter b n 1))




;2
(define (fib n)
  (define (iter a1 a2 n max)
    (if (>= n max)
        (+ a1 a2)
        (iter (+ a1 a2) a1 (+ n 1) max)))
  
  (if (= n 0)
      0
      (iter 1 0 2 n)))

(define (fibonacciRecursive n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fibonacciRecursive (- n 1)) (fibonacciRecursive (- n 2))))))

(require racket/trace)
(trace fib)
(trace fibonacciRecursive)
(fib 5)
(fibonacciRecursive 5)
(untrace fib)
(untrace fibonacciRecursive)

(define timeIterative (current-inexact-milliseconds))
(fib 30)
(- (current-inexact-milliseconds) timeIterative)
(define timeRecursive (current-inexact-milliseconds))
(fibonacciRecursive 30)
(- (current-inexact-milliseconds) timeRecursive)




;3
(define (myLoop f n)
  (when (> n 0)
    f
    myLoop f (- n 1)))




;4
(define (min-fx-gx f g x)
  (min (f x) (g x)))


(define (combine-fx-gx func f g x)
  (func (f x) (g x)))





;5
(define (f g)
  (g 5))
(define (square x) (* x x))

(f square)
(f (lambda (x) (* x (+ x 2))))
;(f f)
;TODO: Erklaerungen




;6
(define (sum term a next b)
  (define (iter a result)
    (if (>= a b)
        (+ (term a) result)
        (iter (next a) (+ (term a) result))))
  (iter a 0))
;TODO: Beispiele





;7
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

;Problem: nur absolute Differenz wird gemessen, funktioniert micht mit sehr kleinen Werten
(define (good-enough? guess x)
  (< (abs (- (* guess guess) x)) 0.001))
;Loesung: relative Genauigkeit messen
(define (better-good-enough? guess x)
  (< (abs (- 1 (/ (* guess guess) x))) 0.01))