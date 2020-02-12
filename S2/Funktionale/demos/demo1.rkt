#lang racket

;;;;demo1 (a)
;;primitive expressions

;a number
5

;a procedure
+


;;combinations: combound expressions (in prefix notation)
(+ 4 6)

(sqrt 64)

(+ 4 3 2 1)

(+ 30)

(+)

(*)

;Note the difference between    +   and  (+)
+


(expt 2 1500)    ;note that e.g. Java's long data type (8byte) is max. ~ 2^63

(+ (* 2 3) (+ 2 2))

(+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))   ;57

;formatting (align operands) --> ignore linebreaks
(+ 
   (* 3 
      (+ (* 2 4) 
         (+ 3 5))) 
   (+ (- 10 7)
      6))

;;abstraction: naming
(define radius 25)

radius

(define pi 3.14)

(* pi (* radius radius))

(define circ (* 2 pi radius))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;demo1 (b)

;compound procedures

(define (square x) (* x x))

;note: we don't need a return statement.
;(define (square x) (+ x 1) x (* x x))

(square 5)

square

;(square)      ;error (no parameter)

(square (square 2))

; use square as a building block to define new procedures
(define (sum-of-squares x y)
  (+ (square x) (square y)))

(sum-of-squares 3 4)

;use s-o-s to define further procedures
(define (f a)
    (sum-of-squares (+ a 1) (* a 2)))

(f 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;demo1 (c)

;conditional expressions & predicates

;note: we may use (), [], or {}

;cond
(define (abs x)
  (cond [(> x 0) x]
        [(= x 0) 0]
        [(< x 0) (- x)]))

;cond else
(define (abs2 x)
  (cond [(< x 0) (- x)]
        [else x]))

;if
(define (abs3 x)
  (if (< x 0)
      (- x)
      x))

; booleans
#t

#f

;primitive predicates

(< 3 4)
(<= 2 2)

;logical composition operators
(and #t #f)

(or #t #f)

(not #t)




;is it a normal or applicative evaluation model?
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;(test 0 (p))


;;Recursion
#|
;built-in:
(define (zero? n)
  (= n 0))

;built-in
(define (sub1 n)
  (- n 1))
|#

(define (factorial n)
  (if (zero? n) 1
      (* n (factorial (sub1 n)))))

(factorial 5)







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;demo1 (d)

;;newton's method (square root)

;prerequisites

;(define (square x) (* x x))

(define (average x y)
  (/ (+ x y) 2))

;e.g. is true for x is 9, guess is 3.0000001
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

;general strategy
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

;to get started
(define (newton x)
  (sqrt-iter 1.0 x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;demo1 (e)

;block structure
(define (newton2 x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

;lexical scoping
(define (newton3 x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))