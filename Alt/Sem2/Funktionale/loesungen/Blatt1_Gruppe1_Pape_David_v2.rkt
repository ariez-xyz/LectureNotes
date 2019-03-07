#lang racket
(display "1a) code snippets\n")

(define a 3)
(define b 4)

;19
(+ a b (* a b))

;#f
(= a b)

;4 da Bedingung true
(if  (and (> b a) (< b (* a b)))
     b
     a)

;procedure blah blah
/

;error
;(-4)

;negativ 4
(- 4)
-4

;procedure minus und positiv 4
- 4

;16, da a un= 4, aber b = 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))

;6, da b > a
(+ 2 (if (> b a) b a))

;16 da a < b
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))

;7 da + returnt wird weil a < b
((if (< a b) + -) a b)



(display "1b) (sign 42)\n")

(define (sign x)
  (cond ((> x 0) 1)
        ((< x 0) -1)
        (else 0)))

(sign 42)



(display "2) (sumOfSmallerSquares 8 4 2)\n")

(define (sumOfSmallerSquares a b c)
  (cond ((and (> a b) (> a c)) (+ (* b b) (* c c)))
        ((and (> b a) (> b c)) (+ (* a a) (* c c)))
        (else (+ (* a a) (* b b)))))

(sumOfSmallerSquares 8 4 2)



(display "3) and/or demos\n")

(define (shortCircuitHelper a b)
  (display "this piece of code was executed\n")
  (if (< a b)
      #t
      #f))

;only prints the message once
(and (shortCircuitHelper 2 1) (shortCircuitHelper 1 2))

(display "---\n")

;prints it twice
(or (shortCircuitHelper 2 1) (shortCircuitHelper 1 2))



(display "4) call sqrt-iter to get stuck in an infinite loop\n")

;we end up in an infinite loop because of applicative order: we evaluate the else case as well
(define (sqrt-iter guess x)
  (myif (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x  guess)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (* guess guess) x)) 0.001))

(define (myif predicate thenc elsec)
  (cond (predicate thenc)
        (else elsec)))

;(sqrt-iter 1 9)



(display "5) (curt 27)\n")

(define (curt-iter guess x)
  (if (good-enough-curt? guess x)
      guess
      (curt-iter (improve-curt guess x) x)))

(define (good-enough-curt? guess x)
  (< (abs (- (* (* guess guess) guess) x)) 0.001))

(define (improve-curt guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (curt x)
  (curt-iter 1.0 x))

(curt 27)



(display "6) remainder vs. mod\n")

;1
(remainder 10 -3)

;-2
(modulo 10 -3)

;mod and rem return the same thing if both arguments have the same sign, but possibly different things if they don't
;(remainder a b) returns a number n such that n is the difference of a and b*floor(a/b)
;(modulo a b) returns a number n such that b*c + n = a