#lang racket



;;Lambda-calculus


;Example 1
;;  double(x)  <=  2*x;
(define (double  x) (* 2 x))

 (λ (x) (* 2 x))
((λ (x) (* 2 x)) 5) ;application


;Example 2
  (λ (x) (λ (y) (* (+ x y) 2))) 
(((λ (x) (λ (y) (* (+ x y) 2))) 2) 3) ;application

;Example 2 simplified (one lambda with two arguments)
 (λ (x y) (* (+ x y) 2))
((λ (x y) (* (+ x y) 2)) 2 3) ;application

;;;;; Currying

(define (add2 x y)
  (+ x y))

(define (add3 x y z)
  (+ x y z))

(define curried_add3 (curry add3))
(define curried_add3_2 (curry add3 1 2))
(curried_add3_2 6)


;

(define (square x) (* x x))

(map square (list 1 2 3))

;this would be nice, but it doesn't work (nof parameters)
;(define square-mapper (map square))

;Note: only this is allowed:
;(define (square-mapper x) (map square x))
;(square-mapper (list 1 2 3))

;racket provides a procedure for this: curry
(define square-mapper ((curry map) square))

(square-mapper (list 1 2 3))

;another example (multiple arguments)
(define (mult x y) (* x y))
(define mult-foldr ((curry foldr) mult 1))
(mult-foldr (list 1 2 3 4))

;this is what we can also do:
(define sm2
  (λ (x) (map square x)))

(sm2 (list 1 2 3))

;one more
(define (volume a b c) (* a b c))
(define volumePerUnitSquare (curry volume 1 1) )
volumePerUnitSquare 5



;church numerals etc:
;see https://courses.cs.washington.edu/courses/csep505/13sp/lectures/church.rkt





