#lang racket

;;;; STREAMS
;;;; 100% self-made implementation of streams   (no racket/stream required)

;;; the core

;special-form!
;(define-syntax-rule (delay exp)
;  (λ() exp))

;(define (force delayedObject)
;  (delayedObject))

(define empty-s 'S-EMPTY-STREAM)

(define (s-empty? s)
  (eq? s empty-s))

;special-form!
(define-syntax-rule (s-cons a b)
  ;(cons a (delay b))) 
  (cons a (λ() b)))

(define (s-first s)
  (car s))

(define (s-rest s)
  ;(force (cdr s)))
  ((cdr s)))

;;;;;;;;;;;;;;;;;;

;;; tests
(define mys (s-cons 1 2))
(s-first mys)
(s-rest mys)

;;; map, filter, ...
(define (s-ref s n)
  (if (= n 0)
      (s-first s)
      (s-ref (s-rest s) (- n 1))))

;map
(define (s-map proc s)
  (if (s-empty? s)
      empty-s
      (s-cons (proc (s-first s)) (s-map proc (s-rest s)))))

;filter
(define (s-filter p s)
  (cond ((s-empty? s) empty-s)
        ((p (s-first s))
         (s-cons (s-first s)
               (s-filter p (s-rest s))))
        (else (s-filter p (s-rest s)))))

;range (enumerate)
(define (s-range low high)
  (if (> low high)
      empty-s
      (s-cons
       low
       (s-range (+ low 1) high))))


;basic tests
(define s12 (s-cons 1 2))
(define s (s-range 1 10))
(s-empty? (s-rest s))
(s-empty? (s-rest (s-rest s)))

(define (s-display s)
  (if (s-empty? s)
      ""
      (~a (s-first s) "," (s-display (s-rest s)))))

(s-display s)


(s-display (s-filter even? s))
(s-display (s-map (λ(x)(* x x)) s))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;performance tests

;helpers;;;
(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        (( divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))
;;;

;with lists
;(time (first (rest (filter prime? (range 10000 400000))))) ;note the reduced range !

;with our implementation
(time (s-first (s-rest (s-filter prime? (s-range 10000 1000000)))))


;(time (in-range 10000 1000000))
;(time (s-range 10000 1000000))


;;;;;;; 3.51
(define (show x)
  (displayln x)
  x)

(define x (s-map show (s-range 0 10)))
(s-ref x 5)


;;;

