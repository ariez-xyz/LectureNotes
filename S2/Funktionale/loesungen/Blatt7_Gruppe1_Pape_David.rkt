#lang racket
;1
(define (func-list n)
  (if (= n 0)
      (list (lambda (x) x))
      (append (func-list (- n 1)) (list (lambda (x) (+ x n))))))


;2
;x=1, y=2, z=3.
;- (+ 1 2) 3
;- 3 3
;0


;3
;(λx.λy.(λz. + z z) y) 5 m
;(λy.(λz. + z z) y) m
;(λz. + z z) m
; + m m


;4
;(λx.λy. - x (λz. * z 2) y) 7 3


;5
;(λy. - 7 (λz. * z 2) y) 3
;(- 7 (* 3 2))
;(- 7 6)
; 1

(define (g x y)
  (define (f x) (* x 2))
  (- x (f y)))

(g 7 3) ; = 1