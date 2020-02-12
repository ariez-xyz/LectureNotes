#lang racket
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; stream motivation

(define (sum-primes a b)
  (define (iter count accum)
    (cond ((> count b) accum)
          ((prime? count) (iter (+ count 1) (+ count accum)))
          (else (iter (+ count 1) accum))))
  (iter a 0))


(define (sum-primes2 a b [accum 0])
    (cond ((> a b) accum)
          ((prime? a) (sum-primes2 (+ a 1) b (+ a accum)))
          (else (sum-primes2 (+ a 1) b accum))))


(define (sum-primes-ci a b)
  (foldr + 0 (filter prime? (range a (+ b 1)))))


;2nd prime number
(car (cdr (filter prime? (range 1 10000))))


;give me the first prime number > 100,000,000
;(car (filter prime? (range 100000000 ?????)))   ;what value should I put here?

;too small: I won't find a prime number:
;(car (filter prime? (range 100000000 100000005)))

;too big: It will take a while:
(car (filter prime? (range 100000000 100100000)))






;;;; STREAMS (presented in the lecture) ;;;;;

;(require racket/stream) ;;in older versions


;(define s0 (stream-cons 6 7))

;(stream-first (stream-cons 1 2))
;(define sr (stream-rest (stream-cons 1 2)))

(define s (stream 1 2 3))

;(stream-first s)

(stream-first s)
(stream-first (stream-rest s))

(stream-length s)
(stream-ref s 0)
(stream-empty? s)

;ref
(define (mystream-ref s n)
  (if (= n 0)
      (stream-first s)
      (stream-ref (stream-rest s) (- n 1))))

;map
(define (mystream-map proc s)
  (if (stream-empty? s)
      empty-stream
      (stream-cons (proc (stream-first s)) (mystream-map proc (stream-rest s)))))

;filter
(define (mystream-filter p s)
  (cond ((stream-empty? s) empty-stream)
        ((p (stream-first s))
         (stream-cons (stream-first s)
               (mystream-filter p (stream-rest s))))
        (else (mystream-filter p (stream-rest s)))))

;range (enumerate)
(define (mystream-range low high)
  (if (> low high)
      empty-stream
      (stream-cons
       low
       (mystream-range (+ low 1) high))))

;helper-procedure: display
;(define (display-stream s)
;  (stream-for-each displayln s))

(define (display-stream s)  ;TODO: improve
  (if (stream-empty? s)
      "...?"
      (~a (stream-first s) "," (display-stream (stream-rest s)))))

(display-stream (stream-filter even? s))

;;;;;;;


(define do (delay (+ 10 10)))
do
(force do)


;first try (won't work)
(define (s-cons a b)
  (cons a (delay b)))


(s-cons 1 (sleep 2))

;;;;;;;;;;;;;;;;;

;(define xxx (stream-cons 1 2))

#|
;a define doesn't make sense. we need a special-form using define-syntax-rule
(define (mystream-cons a b)
  (cons a (delay b)))               ; TODO...... delay
|# 

(define-syntax-rule (mystream-cons a b)
  (cons a (delay b)))               ; TODO...... delay



(define mys (mystream-cons 1 2))

(define (mystream-first s)
  (car s))

(mystream-first mys)

(define (mystream-rest s)
  (force (cdr s)))                  ; TODO....... force

(mystream-rest mys)

;;;;;;;; streams in action


;;;

(in-range 10)

(time (stream-first (stream-rest (stream-filter prime? (in-range 10000 1000000)))))   ;1,000,000

;with lists
(time (first (rest (filter prime? (range 10000 600000))))) ;note the reduced range !  ;600,000


;;; Implementation of delay and force

#|
;a define doesn't make sense. we need a special-form using define-syntax-rule
(define (mydelay exp)
  (λ() exp))
|#

(define-syntax-rule (mydelay exp)
  (λ() exp))

(define (myforce delayedObject)
  (delayedObject))


;;;; see demo6b.rkt for a 100% self-made stream implementation !!!!!!!!!


;;;; infinite streams!

;helper function
(define (display-stream-limit s limit)
  (define (d s lim)
  (if (or (= lim 0) (stream-empty? s))
      "...?"
      (~a (stream-first s) "," (d (stream-rest s) (- lim 1)))))
  (d s limit))


(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))


(display-stream-limit (stream-filter even? integers) 20)

(define (fibgen a b)
  (stream-cons a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))

(display-stream-limit fibs 10)

(define ones (stream-cons 1 ones))

;;;;; more examples

(define (divisible? x y)
  (= (remainder x y) 0))




;stream of integers that are not divisible by 7
(define no-seven (stream-filter (λ(x) (not (divisible? x 7))) integers))


;very nice example: Sieve of eratosthenes



;;;;;;;bank account
(define (stream-withdraw balance amount-stream)
  (stream-cons
   balance
   (stream-withdraw (- balance (stream-first amount-stream))
                    (stream-rest amount-stream))))
  
(define sAccount (stream-withdraw 100 (stream 20 30)))
(display-stream-limit sAccount 3)

